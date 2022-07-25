{- |
Module: Plutus.Contract.PartialTx
Description: Machinery for constructing a 'PartialTx' out of an 'UnbalancedTx', or lookups and constraints.

This module defines functions and types to allow your Haskell 'Contract's to be converted into a declarative
form that can be interpreted using a Lucid on the frontend. Effectively giving you a free deployment strategy
without having to write your offchain code in some frontend framework.

TODO: Property tests for proving equivalence between a given 'UnbalancedTx' and 'PartialTx'.
TODO: FromJSON/ToJSON sanity for 'PartialTx' and all its fields.
TODO: 'Integer' JSON handling - `number` is not sufficient for some fields, `bigint` is necessary.
TODO: Support validity range.
TODO: Test this machinery with multi-minting policy transactions.
-}
module Plutus.Contract.PartialTx (
  PartialTx (..),
  UnbalancedTx (..),
  PartialTxIn (..),
  PartialTxInDetailed (..),
  PartialTxOut (..),
  PartialTxMintVal (..),
  Addr (..),
  Cred (..),
  StakePtr (..),
  AssetId,
  mkPartialTx,
  unbalancedToPartial,
) where

import Data.Aeson (ToJSON (toJSON), (.=))
import qualified Data.Aeson as Aeson
import Data.Coerce (coerce)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as Txt
import Data.Vector (Vector)
import qualified Data.Vector as Vec

import Ledger (
  Address (Address),
  CurrencySymbol,
  Datum,
  MintingPolicy (MintingPolicy),
  POSIXTimeRange,
  PaymentPubKeyHash (PaymentPubKeyHash),
  PubKeyHash (PubKeyHash),
  Redeemer,
  RedeemerPtr (RedeemerPtr),
  Redeemers,
  Script,
  ScriptTag (Mint),
  TokenName,
  Tx (
    Tx,
    txData,
    txInputs,
    txMint,
    txMintScripts,
    txOutputs,
    txRedeemers
  ),
  TxId (TxId),
  TxIn (TxIn),
  TxInType (
    ConsumePublicKeyAddress,
    ConsumeScriptAddress,
    ConsumeSimpleScriptAddress
  ),
  TxOut (TxOut),
  TxOutRef (TxOutRef),
  Validator (Validator),
  ValidatorHash (..),
  Value,
  mintingPolicyHash,
 )
import Ledger.Constraints (
  MkTxError,
  ScriptLookups,
  TxConstraints,
 )
import qualified Ledger.Constraints as Constraints
import Ledger.Constraints.OffChain (UnbalancedTx (..))
import Ledger.Typed.Scripts (
  ValidatorTypes (DatumType, RedeemerType),
 )
import Ledger.Value (
  CurrencySymbol (CurrencySymbol),
  TokenName (TokenName),
 )
import qualified Ledger.Value as Value
import Plutus.V1.Ledger.Bytes (LedgerBytes (LedgerBytes))
import Plutus.V1.Ledger.Credential (
  Credential (..),
  StakingCredential (StakingHash, StakingPtr),
 )
import PlutusTx (FromData, ToData, toData)
import qualified PlutusTx.AssocMap as PlutusMap

import Utils (serializeScriptCborHex)

-- | Use 'Constraints.mkTx' to create an 'UnbalancedTx' and pass it to 'unbalancedToPartial'.
mkPartialTx ::
  ( FromData (DatumType a)
  , ToData (DatumType a)
  , ToData (RedeemerType a)
  ) =>
  ScriptLookups a ->
  TxConstraints (RedeemerType a) (DatumType a) ->
  Either MkTxError PartialTx
mkPartialTx lookups tx = unbalancedToPartial <$> Constraints.mkTx lookups tx

{- | Strip out only the necessary information from an 'UnbalancedTx' and organize it in a way that is more
comprehensible to Lucid, creating a 'PartialTx'.

It is expected that this 'UnbalancedTx' is a result of calling 'Ledger.Constraints.mkTx' on the lookups and the
constraints.

TODO: The PPOSIXTimeRange needs a more Lucid friendly representation.
-}
unbalancedToPartial :: UnbalancedTx -> PartialTx
unbalancedToPartial
  ( UnbalancedTx
      Tx {txInputs, txOutputs, txMint, txMintScripts, txRedeemers, txData}
      reqSigMap
      utxoIx
      validityRange
    ) =
    PartialTx
      { ptx'inps =
          {- Convert the 'TxIn's into 'PartialTxIn' - effectively using the 'TxOutRef's to fill in
          actual 'TxOut' information. -}
          Set.map
            ( \(TxIn ref inTypeMaybe) ->
                let TxOut addr val _ = utxoIx Map.! ref
                 in PartialTxIn ref (addressToAddr addr) (Value.flattenValue val) $ case inTypeMaybe of
                      -- mkTx always puts the 'TxInType' in its output.
                      Nothing -> error "unbalancedToPartial: TxInType missing"
                      Just (ConsumeScriptAddress v redm datm) ->
                        ScriptTxIn v datm redm
                      Just ConsumePublicKeyAddress -> PubKeyTxIn
                      Just ConsumeSimpleScriptAddress -> SimpleScriptTxIn
            )
            txInputs
      , ptx'outs = partialTxOuts
      , ptx'mint = valToMintAsset mpArr txRedeemers txMint
      , -- Datums unused by any script involved in the transaction.
        ptx'extraDatums = Set.fromList . filter (`Set.notMember` usedDatums) $ Map.elems txData
      , ptx'requiredSignatories = Map.keys reqSigMap
      , ptx'validityRange = validityRange
      }
    where
      mpArr = Vec.fromList $ Set.toList txMintScripts
      -- Convert 'TxOut's to 'PartialTxOut's, which essentially replaces the 'DatumHash'es with the actual 'Datum'.
      partialTxOuts =
        map
          ( \(TxOut a b m) ->
              PartialTxOut (addressToAddr a) b $
                (txData Map.!) <$> m
          )
          txOutputs
      inputDatums =
        Set.fold
          ( \(TxIn _ t) acc -> case t of
              Just (ConsumeScriptAddress _ _ d) -> d : acc
              _ -> acc
          )
          []
          txInputs
      usedDatums =
        Set.fromList $
          foldr
            ( \PartialTxOut {ptxOut'datum} acc -> case ptxOut'datum of
                Just datm -> datm : acc
                _ -> acc
            )
            inputDatums
            partialTxOuts

{- | A concatenation of policy id (currency symbol) and token name, in hex.

This is how Lucid represents its asset ids in the `Assets` type.
-}
type AssetId = String

{- | The core of the machinery. A declarative, brief, yet exhaustive summary of a transaction.
Presented in such a way that it is understandable by Lucid with minimal computation work in the frontend.

It also has custom JSON instances that generally make the fields easier to parse on the frontend.
-}
data PartialTx = PartialTx
  { -- | A set of all the inputs that _must_ be consumed by the transaction.
    ptx'inps :: !(Set PartialTxIn)
  , -- | A list of all the outputs that _must_ be produced by the transaction.
    ptx'outs :: ![PartialTxOut]
  , -- | A map of asset identifier to mint info.
    ptx'mint :: !(Map AssetId PartialTxMintVal)
  , -- | Any extra datums added to the transaction.
    ptx'extraDatums :: !(Set Datum)
  , -- | Wallets that _must_ sign the transaction.
    ptx'requiredSignatories :: ![PaymentPubKeyHash]
  , -- | Validity range of the transaction expressed in POSIXTime - interpretation depends on 'SlotConfig'.
    ptx'validityRange :: !POSIXTimeRange
  }
  deriving stock (Eq, Show)

{- | A 'TxIn' that contains not only the 'TxOutRef', but all information about the referenced 'UTxO'.

Indeed, it is possible to use blockfrost (or whatever else) on the frontend to get these details on the
frontend purely through the 'TxOutRef' - but doing it here saves a lot of time. After all, you _fundamentally_
have access to this information already, no extra queries are required.
-}
data PartialTxIn = PartialTxIn
  { ptxIn'ref :: TxOutRef
  , ptxIn'addr :: Addr
  , ptxIn'val :: [(CurrencySymbol, TokenName, Integer)]
  , ptxIn'detailed :: PartialTxInDetailed
  }
  deriving stock (Eq, Ord, Show)

-- | Detailed information about the type of a 'TxIn', and related info.
data PartialTxInDetailed
  = ScriptTxIn {ptxInDet'script :: Validator, ptxInDet'datm :: Datum, ptxInDet'redm :: Redeemer}
  | PubKeyTxIn
  | SimpleScriptTxIn
  deriving stock (Eq, Ord, Show)

-- | This is really just 'TxOut' that uses 'Addr' rather than 'Address'.
data PartialTxOut = PartialTxOut
  { ptxOut'addr :: Addr
  , ptxOut'val :: Value
  , ptxOut'datum :: Maybe Datum
  }
  deriving stock (Eq, Show)

-- | Information about a mint event: the associated minting policy, the redeemer, and the amount to mint.
data PartialTxMintVal = PartialTxMv
  { ptxMv'script :: MintingPolicy
  , ptxMv'redeemer :: Redeemer
  , ptxMv'amount :: Integer
  }
  deriving stock (Eq, Ord, Show)

instance ToJSON PartialTx where
  toJSON
    ( PartialTx
        ins
        outs
        mintVal
        extraDatums
        reqSigs
        validityRange
      ) =
      Aeson.object
        [ "inps" .= ins
        , "outs" .= outs
        , "mint" .= mintVal
        , "extraDatums" .= extraDatums
        , -- Indeed, the hashes should just be printed raw, as hex strings. No constructor clutter in the JSON.
          "requiredSignatories" .= map (\(PaymentPubKeyHash (PubKeyHash h)) -> LedgerBytes h) reqSigs
        , "validityRange" .= validityRange
        ]

instance ToJSON PartialTxInDetailed where
  toJSON (ScriptTxIn (Validator scrpt) datm redm) =
    Aeson.object
      [ "tag" .= Txt.pack "ScriptTxIn"
      , -- This should match Lucid's `Script` type.
        "validator" .= v1ScriptJson scrpt
      , "datum" .= toData datm
      , "redeemer" .= toData redm
      ]
  toJSON PubKeyTxIn = Aeson.object ["tag" .= Txt.pack "PubKeyTxIn"]
  toJSON SimpleScriptTxIn = Aeson.object ["tag" .= Txt.pack "SimpleScriptTxIn"]

-- | Given a Haskell 'Script', create a JSON matching Lucid's `Script` type.
v1ScriptJson :: Script -> Aeson.Value
v1ScriptJson scrpt = Aeson.object ["type" .= Txt.pack "PlutusV1", "script" .= serializeScriptCborHex scrpt]

instance ToJSON PartialTxMintVal where
  toJSON (PartialTxMv (MintingPolicy scrpt) redm amount) =
    Aeson.object
      [ "policy" .= v1ScriptJson scrpt
      , "redeemer" .= toData redm
      , "amount" .= amount
      ]

instance ToJSON PartialTxIn where
  toJSON (PartialTxIn (TxOutRef (TxId txIdBs) idx) addr val detl) =
    Aeson.object
      [ "txId" .= LedgerBytes txIdBs
      , "txIdx" .= idx
      , "address" .= addr
      , "val" .= flattenedValToAsset val
      , "details" .= detl
      ]

instance ToJSON PartialTxOut where
  toJSON (PartialTxOut addr val datm) =
    Aeson.object
      [ "address" .= addr
      , "val" .= valToAsset val
      , "datum" .= fmap toData datm
      ]

{- | All these types are here to make it easier to expect 'Address' in a more cardano-like form.

This is easier to transform into cardano-multiplatform-lib addresses in the frontend.

In general:-
1. A BaseAddress is one that contains both payment credential and stake credential, where the stake credential
   is a 'StakingHash', _not a 'StakingPtr'_. It's like 'Address', where its stake credential has a
   'Just (StakingHash ...)' value.
2. An EnterpriseAddress is 'Address' with its stake credential field set to 'Nothing'.
3. A PointerAddress is like BaseAddress, but the staking credential is a 'StakingPtr', rather than a 'StakingHash'.
-}
data Addr
  = BaseAddr {addr'primary :: Cred, addr'stakeCred :: Cred}
  | EnterpriseAddr {addr'primary :: Cred}
  | PointerAddr {addr'primary :: Cred, addr'stakePtr :: StakePtr}
  deriving stock (Eq, Ord, Show)

data Cred = PubKeyCred PubKeyHash | ScriptCred ValidatorHash
  deriving stock (Eq, Ord, Show)

data StakePtr = StakePtr {stkPtr'slot :: Integer, stkPtr'txIdx :: Integer, stkPtr'certIdx :: Integer}
  deriving stock (Eq, Ord, Show)

addressToAddr :: Address -> Addr
addressToAddr (Address cred Nothing) = EnterpriseAddr $ credentialToCred cred
addressToAddr (Address cred (Just (StakingHash stakeCred))) =
  credentialToCred cred `BaseAddr` credentialToCred stakeCred
addressToAddr (Address cred (Just (StakingPtr a b c))) = PointerAddr (credentialToCred cred) $ StakePtr a b c

credentialToCred :: Credential -> Cred
credentialToCred (PubKeyCredential pkh) = PubKeyCred pkh
credentialToCred (ScriptCredential vh) = ScriptCred vh

instance ToJSON StakePtr where
  toJSON (StakePtr a b c) =
    Aeson.object
      [ "slot" .= a
      , "txIdx" .= b
      , "certIdx" .= c
      ]

instance ToJSON Addr where
  toJSON (BaseAddr a b) =
    Aeson.object
      [ "tag" .= Txt.pack "Base"
      , "primary" .= a
      , "stake" .= b
      ]
  toJSON (EnterpriseAddr a) =
    Aeson.object
      [ "tag" .= Txt.pack "Enterprise"
      , "primary" .= a
      ]
  toJSON (PointerAddr a b) =
    Aeson.object
      [ "tag" .= Txt.pack "Pointer"
      , "primary" .= a
      , "stake" .= b
      ]

instance ToJSON Cred where
  toJSON (PubKeyCred (PubKeyHash h)) =
    Aeson.object
      [ "type" .= Txt.pack "Key"
      , "hash" .= LedgerBytes h
      ]
  toJSON (ScriptCred (ValidatorHash h)) =
    Aeson.object
      [ "type" .= Txt.pack "Script"
      , "hash" .= LedgerBytes h
      ]

-- | Convert a 'Value' to Lucid's `Assets` type.
valToAsset :: Value -> Map AssetId Integer
valToAsset =
  flattenedValToAsset
    . Value.flattenValue

-- | Convert a flattened 'Value' to Lucid's `Assets` type.
flattenedValToAsset :: [(CurrencySymbol, TokenName, Integer)] -> Map AssetId Integer
flattenedValToAsset =
  foldr
    ( \(CurrencySymbol cs, TokenName tk, i) acc ->
        let k' = show (LedgerBytes cs) ++ show (LedgerBytes tk)
            -- "lovelace" denotes the ada asset name.
            k = if null k' then "lovelace" else k'
         in Map.insertWith (+) k i acc
    )
    mempty

-- | Similar to 'valToAsset', but the value in the maps contain extra minting information.
valToMintAsset :: Vector MintingPolicy -> Redeemers -> Value -> Map AssetId PartialTxMintVal
valToMintAsset mps redmMap v =
  foldMap
    ( \cs ->
        let (mp, redm) = mpInfo Map.! Value.currencyMPSHash cs
            tokMap = Map.fromList . PlutusMap.toList . fromMaybe PlutusMap.empty $ PlutusMap.lookup cs valMap
            csHex = LedgerBytes $ coerce cs
         in Map.foldMapWithKey
              ( \(TokenName tkn) i ->
                  Map.singleton (show csHex ++ show (LedgerBytes tkn)) $
                    PartialTxMv mp redm i
              )
              tokMap
    )
    $ Set.toList uniqueSyms
  where
    uniqueSyms = Set.fromList $ Value.symbols v
    valMap = Value.getValue v
    {- Convert the 'Redeemers' map into a Map from currency symbol to Minting Policy + Redeemer.

    This is way easier to perform lookups on.
    -}
    mpInfo =
      Map.foldMapWithKey
        ( \(RedeemerPtr tag idx) redm ->
            case tag of
              Mint -> let mp = mps Vec.! fromInteger idx in Map.singleton (mintingPolicyHash mp) (mp, redm)
              _ -> mempty
        )
        redmMap
