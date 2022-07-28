# Plutus Partial Tx

A library to export partial transactions (unbalanced, unsigned) and get them
signed and submitted by a frontend PAB. This enables you to re-use your regular
Haskell `Contract`s while still having a deployment solution for production -
effortlessly.

# Highlights

- Write all your contracts in the familiar `Contract` monad.
- Leverage excellent Haskell-side testing solutions for your `Contract`s, using
  [Plutip](https://github.com/mlabs-haskell/plutip), Contract models etc.
- Run your contracts on the testnet (and mainnet!) utilizing a lightweight
  frontend PAB (i.e [Lucid](https://github.com/Berry-Pool/lucid)) - without ever
  having to rewrite your contracts or any extra logic. Simply re-use your
  Haskell contracts, call `.sign` and `.submit` on the frontend.
- Extremely lightweight, easy to test in local development environment. If using
  [bot-plutus-interface](https://github.com/mlabs-haskell/bot-plutus-interface)
  (BPI), all you need is `cardano-node` and `plutus-chain-index`.

# Installation

1. Add a `bot-plutus-interface` as a dependency in your nix flake setup.
2. Add `plutus-partial-tx` to your flake inputs and extra sources.
3. Add latest version of `cardano-node`, `cardano-cli` to your nix shell.

   (OR) Download them from hydra and put them in your `$PATH`.
4. Have `plutus-chain-index` (ideally the same version used by `bot-plutus-interface`) binary available in your shell/PATH.
5. Install `lucid-cardano-partialtx` via NPM.

# Usage

## Backend environment setup
1. Add the proper versions of `bot-plutus-interface` and `plutus-partial-tx` as dependencies to your project.
2. Have the proper versions (usually latest) of `cardano-node`, `cardano-cli`, and a corresponding version (same one used by bot-plutus-interface) of `plutus-chain-index` in `$PATH`.
3. Have `cardano-node` and `chain-index` running in the background and properly synced. You may simply copy over [the provided `testnet` directory](./testnet/) and use the scripts there to set up for testnet.

## Frontend environment setup

How you set up the frontend is entirely upto you, as long as it can query the Haskell server to obtain a `PartialTx` - and use it with `lucid-cardano` and `lucid-cardano-partialtx`, it's enough.

See [`Berry-Pool/lucid`](https://github.com/Berry-Pool/lucid) for adding Lucid to your dependency.

For `lucid-cardano-partialtx` (provided by this repo), there are three ways to import it:

### Node.js

Install through `npm`:

```sh
npm install lucid-cardano-partialtx
```

Import in your file:

```sh
import { buildTxFrom } from "lucid-cardano-partialtx";
```

> Aside: You can use webpack or similar to bundle your Node project to run on the browser. See [full example that does this](#full-example-and-how-to-run-it)

### Deno

Simply import from deno.land:

```ts
import { Lucid } from "https://deno.land/x/lucid-partialtx@0.1.0/mod.ts";
```

> Aside: You can use ESBuild or similar to bundle your Deno project to run on the browser. However, you should to replace the `deno.land` imports with the [browser package](#browser-js) url if running in a browser environment. See [lucid-partialtx/build.ts](./lucid-partialtx/build.ts) that does something similar (but only after generating a node package).

### Browser JS

<script type="module">

import { Lucid } from "https://unpkg.com/lucid-cardano-partialtx@0.1.2/web/mod.js";

</script>

> Aside: This is pure JS directly running in the browser: probably not too practical for large projects.

## Haskell server

Once you have the environment set up, your haskell server merely has to use BPI (bot-plutus-interface) to run your contracts returning `PartialTx`s. You can hook up BPI with the testnet quite easily: simply copy over the [`BPI.Testnet.Setup` module](./example/BPI/Testnet/Setup.hs) from the example.

A simple servant server, showcasing a simple `Contract` usage, can be found in [example/Main](./example/Main.hs).

## Frontend Lucid

All you have to do is create a `PartialTxInterpreter` by passing your `Lucid` instance to `mkPartialTxInterpreter`, make an API call to receieve the `PartialTx` from your server, and pass it through the interpreter. The resulting Lucid `Tx` can be modified further or directly signed, and submitted.

See [example/frontend/src/index.ts](./example/frontend/src/index.ts).

# Full example and how to run it

There is a full example with servant, BPI, and Lucid that can run a dummy
minting contract on the testnet. Check the haskell code
[in the example directory](./example). The gist is that you can copy over
`BPI.Testnet.Setup` and use the exposed interface to create `PartialTx`s in the
context of the testnet.

The example frontend is in [`example/frontend`](./example/frontend).

To run the project and run stuff on the testnet, head inside the nix shell by
doing `nix develop`, and follow these steps:

## 1. Fill in blockfrost config

You need to fill a `config.json` and put it in `example/frontend/config.json`, this
config should contain your blockfrost API access key:

```json
{
  "blockfrostUrl": "TESTNET_BLOCKFROST_URL",
  "blockfrostProjId": "TESTNET_BLOCKFROST_PROJID"
}
```

> Aside: Of course, you can also deploy all this in production by switching out
> to connect to the mainnet in the BPI setup and blockfrost setup.

## 2. Start `cardano-node` and `plutus-chain-index` in background

You'll also need `cardano-node` and `chain-index` running in the background,
properly connected to testnet.

All you have to do run `make services`.

This will take some time to sync. You can see the node logs in
`testnet/node.log` and chain index logs in `testnet/cix.log`. You can query the
node sync progress by running `make query-tip`.

> Note: Remember to stop these background services when you're done! Use
> `make stop-services` to do so.

## 3. Build the frontend project

Head inside the `example/frontend` directory and run the following commands:

- `npm i` - to install all the npm dependencies
- `npx webpack` - to build the project

Alternatively, if you've already done `npm i` and have the `node_modules` from
it - you can run `make build-frontend` from the root project path.

## 4. Start the server

Once the node has synced and all the previous steps have been completed, run
`make serve`. Head to `localhost:8080` to see a beautiful frontend with a
singular dummy minting button!

**Note**: You'll need a wallet supporting Vasil for signing and submission to
work properly. In particular, mainline Nami does not support vasil yet. You need to use the [`vasil` branch of nami](https://github.com/Berry-Pool/nami-wallet/tree/vasil#testnet).

# API

Here's the core idea:

Keep your Haskell contracts as they are, just make them return `UnbalancedTx`
using
[`Ledger.Constraints.mkTx`](https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-constraints/html/Ledger-Constraints.html#v:mkTx).
Alternatively, make them return the `ScriptLookups` and the `TxConstraints`.

Use either of the two functions provided within this repo to create a
`PartialTx`:

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

> Aside: For your Haskell side tests (Plutip or EmulatorTrace), you'd still want
> to submit the `UnbalancedTx` - which you can still do using
> [`submitUnbalancedTx`](https://playground.plutus.iohkdev.io/doc/haddock/plutus-contract/html/Plutus-Contract.html#v:submitUnbalancedTx).
> So calling `mkTx` followed by `submitUnbalancedTx` is effectively the same as
> just calling `submitTx` (and similar).

In the frontend, bind your `Lucid` instance to `mkPartialTxInterpreter`, and use the resulting `PartialTxInterpreter` to interpret a `PartialTx`:

```ts
type PartialTxInterpreter = (x: PartialTx) => Tx;

mkPartialTxInterpreter(lucid: Lucid): PartialTxInterpreter;
```

Once you obtain a Lucid `Tx`, it's as simple as signing and submitting it, which
looks like:

```ts
const signedTx = await tx.sign().complete();
return signedTx.submit();
```
