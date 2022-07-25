module Utils (serializeScriptCborHex) where

import Codec.Serialise (serialise)
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import Data.Text (Text)
import qualified Data.Text.Encoding as Txt

import qualified Cardano.Binary as CBOR
import Plutus.V1.Ledger.Scripts (Script)

-- | A showable hex string representing a serialized script in CBOR.
serializeScriptCborHex :: Script -> Text
serializeScriptCborHex =
  Txt.decodeUtf8 . Base16.encode . CBOR.serialize' . SBS.toShort . LBS.toStrict
    . serialise
