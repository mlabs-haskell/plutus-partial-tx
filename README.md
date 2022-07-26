# Plutus Partial Tx

A library to export partial transactions (unbalanced, unsigned) and get them signed and submitted by a frontend PAB. This enables you to re-use your regular Haskell `Contract`s while still having a deployment solution for production - effortlessly.

# Highlights

- Write all your contracts in the familiar `Contract` monad.
- Leverage excellent Haskell-side testing solutions for your `Contract`s, using [Plutip](https://github.com/mlabs-haskell/plutip), Contract models etc.
- Run your contracts on the testnet (and mainnet!) utilizing a lightweight frontend PAB (i.e [Lucid](https://github.com/Berry-Pool/lucid)) - without ever having to rewrite your contracts or any extra logic. Simply re-use your Haskell contracts, call `.sign` and `.submit` on the frontend.
- Extremely lightweight, easy to test in local development environment. If using [bot-plutus-interface](https://github.com/mlabs-haskell/bot-plutus-interface) (BPI), all you need is `cardano-node` and `plutus-chain-index`.

# Usage

See: [full example](#full-example)

Here's the core idea:

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
buildTxFrom(this: Lucid, { inps, outs, mint, requiredSignatories, extraDatums }: PartialTx): Tx
```

> Aside: For now, you may have to copy over `lucid-partialtx` to use `buildTxFrom` and relevant types. In the future, there will be an upstream package for it.

Once you obtain a Lucid `Tx`, it's as simple as signing and submitting it, which looks like:

```ts
const tx = await buildTxFrom.bind(lucid, partialTx).complete();
const signedTx = await tx.sign().complete();
return signedTx.submit();
```

where `lucid` is an instance of `Lucid`, and `partialTx` is the `PartialTx` JSON.

# Full example
There is also a full example with servant, BPI, and Lucid that can run a dummy minting contract on the testnet. Check the haskell code [in the example directory](./example). The gist is that you can copy over `BPI.Testnet.Setup` and use the exposed interface to create `PartialTx`s in the context of the testnet.

The example frontend is in `lucid-partialtx/example`.

To run the project and run stuff on the testnet, head inside the nix shell by doing `nix develop`, and follow these steps:

## 1. Fill in blockfrost config
 You need to fill a `config.json` and put it in `example/config.json`, this config should contain your blockfrost API access key:

```json
{
    "blockfrostUrl": "TESTNET_BLOCKFROST_URL",
    "blockfrostProjId": "TESTNET_BLOCKFROST_PROJID"
}
```

> Aside: Of course, you can also deploy all this in production by switching out to connect to the mainnet in the BPI setup and blockfrost setup.

## 2. Start `cardano-node` and `plutus-chain-index` in background

You'll also need `cardano-node` and `chain-index` running in the background, properly connected to testnet.

All you have to do run `make services`.

This will take some time to sync. You can see the node logs in `testnet/node.log` and chain index logs in `testnet/cix.log`. You can query the node sync progress by running `make query-tip`.

> Note: Remember to stop these background services when you're done! Use `make stop-services` to do so.

## 3. Build the frontend project
Head inside the `lucid-partialtx` directory and run the following commands:

- `npm i` - to install all the npm dependencies
- `npx webpack` - to build the project

Alternatively, if you've already done `npm i` and have the `node_modules` from it - you can run `make build-frontend` from the root project path.

## 4. Start the server

Once the node has synced and all the previous steps have been completed, run `make serve`. Head to `localhost:8080` to see a beautiful frontend with a singular dummy minting button!

**Note**: You'll need a wallet supporting Vasil for signing and submission to work properly.
