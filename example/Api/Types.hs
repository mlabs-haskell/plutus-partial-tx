{-# OPTIONS_GHC -Wno-orphans #-}

module Api.Types (
  ExampleApi,
  NamedExampleApi (..),
  StaticRoutes (..),
  ContractRoutes (..),
  HexStringOf,
  fromHexString,
  toHexString,
) where

import Data.Coerce (Coercible, coerce)

import Servant (
  Header,
  Header',
  JSON,
  NamedRoutes,
  Post,
  Raw,
  ReqBody,
  Required,
  Strict,
  type (:<|>),
  type (:>),
 )
import Servant.API.Generic (Generic, GenericMode (type (:-)))

import Ledger.Address (StakePubKeyHash)
import Plutus.V1.Ledger.Api (
  BuiltinByteString,
  LedgerBytes (LedgerBytes),
  PubKeyHash,
 )

import Plutus.Contract.PartialTx (PartialTx)

-- | 'NamedExampleApi' alongside the raw index (`/`) serve route.
type ExampleApi = NamedRoutes NamedExampleApi :<|> Raw

-- | All the example routes _except_ the index (i.e `/`).
data NamedExampleApi mode = NamedExampleApi
  { staticRoutes :: mode :- NamedRoutes StaticRoutes
  , contractRoutes ::
      mode :- "contracts"
        :> Header' '[Required, Strict] "OwnPubKeyHash" (HexStringOf PubKeyHash)
        :> Header "OwnStakePubKeyHash" (HexStringOf StakePubKeyHash)
        :> NamedRoutes ContractRoutes
  }
  deriving stock (Generic)

-- | The `dist/` static asset serving route.
newtype StaticRoutes mode = StaticRoutes {dist :: mode :- "dist" :> Raw}
  deriving stock (Generic)

data ContractRoutes mode = ContractRoutes
  { dummyMintA ::
      mode :- "dummyMintA"
        :> ReqBody '[JSON] ()
        :> Post '[JSON] PartialTx
  }
  deriving stock (Generic)

-- | A hex string with a phantom type indicating its purpose.
type HexStringOf a = LedgerBytes

fromHexString :: Coercible BuiltinByteString a => HexStringOf a -> a
fromHexString (LedgerBytes h) = coerce h

toHexString :: Coercible a BuiltinByteString => a -> HexStringOf a
toHexString h = LedgerBytes $ coerce h
