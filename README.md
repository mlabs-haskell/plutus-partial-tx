# Plutus Partial Tx

A library to export partial transactions (unbalanced, unsigned) and get them signed and submitted by a frontend PAB. This enables you to re-use your regular Haskell `Contract`s while still having a deployment solution for production - effortlessly.

**NOTE**: ~~This currently lacks support for validity ranges.~~ Please check out the `vasil` branch.

# Highlights

- Write all your contracts in the familiar `Contract` monad.
- Leverage excellent Haskell-side testing solutions for your `Contract`s, using [Plutip](https://github.com/mlabs-haskell/plutip), Contract models etc.
- Run your contracts on the testnet (and mainnet!) utilizing a lightweight frontend PAB (i.e [Lucid](https://github.com/Berry-Pool/lucid)) - without ever having to rewrite your contracts or any extra logic. Simply re-use your Haskell contracts, call `.sign` and `.submit` on the frontend.
- Extremely lightweight, easy to test in local development environment. If using [bot-plutus-interface](https://github.com/mlabs-haskell/bot-plutus-interface) (BPI), all you need is `cardano-node` and `plutus-chain-index`.

# Usage

There will soon be a full example in this repo once BPI has been updated with Vasil support. But here's the core idea:

Keep your Haskell contracts as they are, just make them return `UnbalancedTx` using [`Ledger.Constraints.mkTx`](https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-constraints/html/Ledger-Constraints.html#v:mkTx). Alternatively, make them return the `ScriptLookups` and the `TxConstraints`.

Use either of the two functions provided within this repo to create a `PartialTx`:

```hs
import Plutus.Contract.PartialTx

mkPartialTx ::
  ( FromData (DatumType a)
  , ToData (DatumType a)
  , ToData (RedeemerType a)
  ) =>
  ScriptLookups a ->
  TxConstraints (RedeemerType a) (DatumType a) ->
  Either MkTxError PartialTx

unbalancedToPartial :: UnbalancedTx -> PartialTx
```

> Aside: For your Haskell side tests (Plutip or EmulatorTrace), you'd still want to submit the `UnbalancedTx` - which you can still do using [`submitUnbalancedTx`](https://playground.plutus.iohkdev.io/doc/haddock/plutus-contract/html/Plutus-Contract.html#v:submitUnbalancedTx). So calling `mkTx` followed by `submitUnbalancedTx` is effectively the same as just calling `submitTx` (and similar).

Now, you need a Haskell server to serve these `PartialTx` to the frontend - where the user's browser will receive the JSON of this data type. Once the frontend has access to the `PartialTx`, you can use Lucid to interpret it into a Transaction:

```ts
buildTxFrom({ inps, outs, mint, requiredSignatories, extraDatums }: PartialTx): Tx
```

> Aside: For now, you may have to copy over `lucid-partialtx` to use `buildTxFrom` and relevant types. In the future, there will be an upstream package for it.

Once you obtain a Lucid `Tx`, it's as simple as signing and submitting it, which looks like:

```ts
const tx = await lucid.buildTxFrom(partialTx).complete();
const signedTx = await tx.sign().complete();
return signedTx.submit();
```

where `lucid` is an instance of `LucidEx` from `lucid-partialtx`, and `partialTx` is the `PartialTx` JSON.
