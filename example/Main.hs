{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Main (main) where

import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Text.IO as TxtIO
import Data.Void (Void)

import Network.Wai.Handler.Warp (Port)
import qualified Network.Wai.Handler.Warp as Warp
import Servant

import Ledger (StakePubKeyHash (StakePubKeyHash), scriptCurrencySymbol)
import qualified Ledger.Constraints as Constraints
import Plutus.Contract (AsContractError, Contract, EmptySchema)
import Plutus.V1.Ledger.Crypto (PubKeyHash (PubKeyHash))
import Plutus.V1.Ledger.Scripts (MintingPolicy (MintingPolicy), Script (Script))
import Plutus.V1.Ledger.Value (AssetClass (AssetClass), CurrencySymbol)
import qualified Plutus.V1.Ledger.Value as Value
import qualified PlutusCore as PLC
import UntypedPlutusCore

import BPI.Testnet.Setup (ContractRunner (..))
import qualified BPI.Testnet.Setup as BpiSetup
import Plutus.Contract.PartialTx (
  PartialTx,
  UnbalancedTx,
  unbalancedToPartial,
 )

import Api.Types (
  ContractRoutes (ContractRoutes),
  ExampleApi,
  HexStringOf,
  NamedExampleApi (NamedExampleApi, contractRoutes, staticRoutes),
  StaticRoutes (StaticRoutes, dist),
  fromHexString,
 )

-- | "Always succeeds" 'MintingPolicy' UPLC logic; for testing use.
alwaysSucceedsUPLC :: Program DeBruijn DefaultUni DefaultFun ()
alwaysSucceedsUPLC =
  Program () (Version () 1 0 0) $
    LamAbs
      ()
      (DeBruijn 0)
      ( LamAbs
          ()
          (DeBruijn 0)
          (Constant () (PLC.Some (PLC.ValueOf PLC.DefaultUniUnit ())))
      )

-- | Always succeeds 'MintingPolicy'; for testing use.
dummyTkMp :: MintingPolicy
dummyTkMp = MintingPolicy $ Script alwaysSucceedsUPLC

-- | 'CurrencySymbol' of 'dummyTkMp', a 'MintingPolicy' being used for testing.
dummyTkMpCs :: CurrencySymbol
dummyTkMpCs = scriptCurrencySymbol dummyTkMp

-- | First dummy 'AssetClass' used in instrument testing as liquidity token.
aAssetCls :: AssetClass
aAssetCls = AssetClass (dummyTkMpCs, "A")

-- | Dummy contract to mint testing tokens to be used as liquidity.
dummyMint :: AssetClass -> Integer -> Contract w s e UnbalancedTx
dummyMint ascls amount = do
  let val = Value.assetClassValue ascls amount
      lookups = Constraints.plutusV1MintingPolicy dummyTkMp
      tx = Constraints.mustMintValue val
  either undefined pure $ Constraints.mkTx @Void lookups tx

data ExampleCtx = ExampleCtx
  { indexContent :: ByteString
  , contrRunner :: ContractRunner
  }

contractEndpoints ::
  _ =>
  ExampleCtx ->
  HexStringOf PubKeyHash ->
  Maybe (HexStringOf StakePubKeyHash) ->
  ContractRoutes mode
contractEndpoints ExampleCtx {contrRunner = ContractRunner {runContract}} ownPkh' ownSkh' =
  ContractRoutes $ dummyMintHandler aAssetCls
  where
    ownPkh = fromHexString ownPkh'
    ownSkh = fromHexString <$> ownSkh'
    rc :: AsContractError e => Contract () EmptySchema e a -> IO (Either e a)
    rc = runContract @() (ownPkh, ownSkh)
    dummyMintHandler :: AssetClass -> () -> Handler PartialTx
    dummyMintHandler asCls _ = do
      exOutcome <-
        liftIO
          . rc
          $ dummyMint asCls 100
      case exOutcome of
        Left err -> liftIO (TxtIO.putStrLn err) *> throwError err400
        Right ubTx -> pure (unbalancedToPartial ubTx)

server :: ExampleCtx -> Server ExampleApi
server eCtx@ExampleCtx {indexContent} =
  NamedExampleApi
    { staticRoutes = StaticRoutes {dist = serveDirectoryWebApp "example/frontend/dist"}
    , contractRoutes = contractEndpoints eCtx
    }
    :<|> serveDirectoryEmbedded [("index.html", indexContent)]

main :: IO ()
main = do
  let bpiDir = "testnet"

  contrRunner <- BpiSetup.runSetup bpiDir
  indexContent <- liftIO $ BS.readFile "example/frontend/index.html"

  let port :: Port = 8080
  putStrLn $ "Serving at port: " ++ show port
  Warp.run 8080
    . serveWithContext @ExampleApi Proxy EmptyContext
    . server
    $ ExampleCtx indexContent contrRunner
