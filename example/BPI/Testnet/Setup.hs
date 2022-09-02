module BPI.Testnet.Setup (ContractRunner (ContractRunner, runContract), runSetup) where

import Control.Concurrent.STM (newTVarIO)
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import qualified Data.Text as Txt
import qualified Data.UUID.V4 as UUID
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Numeric.Natural (Natural)
import Servant.Client (BaseUrl (BaseUrl), Scheme (Http))
import System.Directory (createDirectoryIfMissing)
import System.Environment (getEnv)
import System.FilePath ((</>))

import qualified BotPlutusInterface.CardanoNode.Query as BPI
import qualified BotPlutusInterface.Contract as BPI
import BotPlutusInterface.Types (
  CLILocation (Local),
  CollateralVar (CollateralVar),
  ContractEnvironment (..),
  ContractState (ContractState),
  LogLevel (Info),
  LogType (AnyLog),
  PABConfig (..),
  TxStatusPolling (TxStatusPolling),
 )
import Cardano.Api (CardanoMode, ConsensusModeParams (CardanoModeParams), EpochSlots (EpochSlots), QueryInShelleyBasedEra (QueryProtocolParameters))
import Cardano.Api.Shelley (
  LocalNodeConnectInfo (LocalNodeConnectInfo),
  NetworkId (Testnet),
  NetworkMagic (NetworkMagic),
  ProtocolParameters,
  ToJSON,
 )
import Control.Monad.Freer (runM)
import Control.Monad.Freer.Reader (runReader)
import Ledger (PubKeyHash, StakePubKeyHash)
import Plutus.Contract (AsContractError, Contract, EmptySchema)
import Plutus.PAB.Core.ContractInstance.STM (Activity (Active))
import Wallet.Types (ContractInstanceId (ContractInstanceId))

-- | default collateral size that's to be used as collateral.
defCollateralSize :: Natural
defCollateralSize = 10_000_000

pParamsFile :: FilePath
pParamsFile = "pparams.json"

keysDir :: FilePath
keysDir = "signing-keys"

scriptsDir :: FilePath
scriptsDir = "scripts"

txsDir :: FilePath
txsDir = "txs"

metadataDir :: FilePath
metadataDir = "metadata"

newtype ContractRunner = ContractRunner
  { runContract ::
      forall w e a.
      (ToJSON w, Monoid w, AsContractError e) =>
      (PubKeyHash, Maybe StakePubKeyHash) ->
      Contract w EmptySchema e a ->
      IO (Either e a)
  }

-- | Creates directories necessary for bot interface and creates the contract env for testnet.
runSetup :: FilePath -> IO ContractRunner
runSetup workDir = do
  setLocaleEncoding utf8
  createRequiredDirs
  sockPath <- getEnv "CARDANO_NODE_SOCKET_PATH"
  let nodeInfo = LocalNodeConnectInfo
                   (CardanoModeParams $ EpochSlots (60 * 60 * 24))
                   (Testnet $ NetworkMagic 2)
                   sockPath
  (pparams, paramsFile) <- saveProtocolParams nodeInfo

  let pabConf (ownPkh, ownSpkh) = mkPabConf pparams (Txt.pack paramsFile) workDir (ownPkh, ownSpkh)
  pure $
    ContractRunner $ \(pkh, spkh) contr -> do
      instId <- ContractInstanceId <$> UUID.nextRandom
      st <- newTVarIO (ContractState Active mempty)
      a <- newTVarIO mempty
      b <- newTVarIO mempty
      c <- CollateralVar <$> newTVarIO Nothing
      let cEnv = ContractEnvironment (pabConf (pkh, spkh)) instId st a b c
      BPI.runContract cEnv contr
  where
    createRequiredDirs =
      mapM_
        (createDirectoryIfMissing True . (workDir </>))
        [ keysDir
        , scriptsDir
        , txsDir
        , metadataDir
        ]
    saveProtocolParams :: LocalNodeConnectInfo CardanoMode -> IO (ProtocolParameters, FilePath)
    saveProtocolParams nodeInfo = do
      pparams :: ProtocolParameters <-
        getOrFailM
          . runM
          . runReader nodeInfo
          $ BPI.queryBabbageEra QueryProtocolParameters
      let ppath = workDir </> pParamsFile
      Aeson.encodeFile ppath pparams
      return (pparams, ppath)

getOrFail :: Show e => Either e a -> a
getOrFail = either (error . show) id

getOrFailM :: (Show e, Functor f) => f (Either e b) -> f b
getOrFailM = (getOrFail <$>)

mkPabConf :: ProtocolParameters -> Text -> FilePath -> (PubKeyHash, Maybe StakePubKeyHash) -> PABConfig
mkPabConf pparams pparamsFile workDir (ownPkh, ownSpkh) =
  PABConfig
    { pcCliLocation = Local
    , pcNetwork = Testnet (NetworkMagic 2)
    , pcChainIndexUrl = BaseUrl Http "localhost" 9083 ""
    , pcPort = 9080
    , pcProtocolParams = Just pparams
    , pcTipPollingInterval = 1_000_000
    , pcOwnPubKeyHash = ownPkh
    , pcOwnStakePubKeyHash = ownSpkh
    , pcScriptFileDir = Txt.pack $ workDir </> scriptsDir
    , pcSigningKeyFileDir = Txt.pack $ workDir </> keysDir
    , pcTxFileDir = Txt.pack $ workDir </> txsDir
    , pcDryRun = False
    , pcCollectLogs = False
    , pcBudgetMultiplier = 1
    , pcTxStatusPolling = TxStatusPolling 500_000 5
    , pcLogLevel = Info [AnyLog]
    , pcProtocolParamsFile = pparamsFile
    , pcEnableTxEndpoint = True
    , pcCollectStats = True
    , pcMetadataDir = Txt.pack $ workDir </> metadataDir
    , pcCollateralSize = defCollateralSize
    }
