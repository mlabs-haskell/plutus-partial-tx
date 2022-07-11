import { PolicyId, TxHash } from 'lucid-cardano';

// JSON representation of 'PlutusTx.Rational' on the Haskell side.
export type Rational = { numerator: number; denominator: number };

// JSON representation of 'TxOutRef' on the Haskell side.
export type TxOutRef = { txOutRefId: { getTxId: TxHash }; txOutRefIdx: number };

// JSON representation of 'CurrencySymbol' on the Haskell side.
export type CurrencySymbol = { unCurrencySymbol: PolicyId };

// JSON representation of 'TokenName' on the Haskell side.
export type TokenName = { unTokenName: string };

// JSON representation of 'AssetClass' on the Haskell side.
export type AssetClass = { unAssetClass: [CurrencySymbol, TokenName] };

// Build the JSON form of 'TxOutRef' (Haskell) out of the tx id and output index.
export function txOutRef(txId: TxHash, outIdx: number): TxOutRef {
  return { txOutRefId: { getTxId: txId }, txOutRefIdx: outIdx };
}

// Build the JSON form of 'AssetClass' (Haskell) out of the curr sym and tk name.
export function assetClass(cs: PolicyId, tkName: string): AssetClass {
  return { unAssetClass: [{ unCurrencySymbol: cs }, { unTokenName: tkName }] };
}
