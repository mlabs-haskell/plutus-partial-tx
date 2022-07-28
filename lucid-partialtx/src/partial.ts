/**
 * This is the `PartialTx` interpreter written in the perspective of Lucid.
 *
 * It receives a `PartialTx` in its JSON form (possibly from a haskell server), and builds a lucid
 * transaction out of it.
 *
 * TODO: 'Integer's from Haskell get converted into 'number' in JSON.
 *        This is a big problem for certain fields, identify them, and come up with an elegant solution for them.
 */

import {
  Address,
  Assets,
  C,
  Credential,
  Datum,
  fromHex,
  Lucid,
  MintingPolicy,
  Network,
  networkToId,
  PaymentKeyHash,
  Redeemer,
  SpendingValidator,
  Tx,
  UTxO,
} from "./deps/lucid.ts";

// This is just 'Assets' from Lucid but uses 'number' rather than 'bigint'.
export type AssetsP = Record<string, number>;

// This is the JSON representation of `Map AssetId PartialTxMint` on the Haskell side.
export type MintAssets = Record<
  string,
  { policy: MintingPolicy; redeemer: Redeemer; amount: number }
>;

// This is the JSON representation of `PartialTxInDetailed` on the Haskell side.
export type TxInDetails =
  | { tag: "PubKeyTxIn" }
  | { tag: "SimpleScriptTxIn" }
  | {
    tag: "ScriptTxIn";
    validator: SpendingValidator;
    datum: Datum;
    redeemer: Redeemer;
  };

// This is the JSON representation of `Addr` on the Haskell side.
export type Addr =
  | { tag: "Base"; primary: Credential; stake: Credential }
  | { tag: "Enterprise"; primary: Credential }
  | {
    tag: "Pointer";
    primary: Credential;
    stake: { slot: number; txIdx: number; certIdx: number };
  };

// This is the JSON representation of `PartialTx` on the Haskell side.
export type PartialTx = {
  inps: {
    txId: string;
    txIdx: number;
    val: AssetsP;
    details: TxInDetails;
    address: Addr;
  }[];
  outs: {
    address: Addr;
    val: AssetsP;
    datum?: Datum;
  }[];
  mint: MintAssets;
  requiredSignatories: PaymentKeyHash[];
  extraDatums: Datum[];
  validityRange: { validFrom?: number; validTo?: number };
};

export type PartialTxInterpreter = (x: PartialTx) => Tx;

// The `PartialTx` interpreter, convert a `PartialTx` into a Lucid transaction.
export function mkPartialTxInterpreter(lucid: Lucid): PartialTxInterpreter {
  return (
    { inps, outs, mint, requiredSignatories, extraDatums, validityRange },
  ) => {
    // Filter out the non-script inputs to 'collectFrom' all at once.
    const simpleInps = inps.flatMap((
      { address, txId, txIdx, val, details },
    ): UTxO[] =>
      details.tag === "ScriptTxIn" ? [] : [
        {
          txHash: txId,
          outputIndex: txIdx,
          address: addrToAddress(lucid.network, address),
          assets: assetsFromAssetsP(val),
        },
      ]
    );

    // Step 1: The base transaction starts off consuming all the simple inputs.
    let tx = lucid.newTx().collectFrom(simpleInps);

    // Step 2: Next, all the script inputs are added into the transaction, alongside their datums and redeemers.
    tx = inps.reduce((acc, { txId, txIdx, address, val, details }) => {
      switch (details.tag) {
        case "ScriptTxIn": {
          const utxo: UTxO = {
            txHash: txId,
            outputIndex: txIdx,
            address: addrToAddress(lucid.network, address),
            assets: assetsFromAssetsP(val),
            datum: details.datum,
            datumHash: lucid.utils.datumToHash(details.datum),
          };
          return acc.collectFrom([utxo], details.redeemer);
        }
        // These have already been added in the previous step.
        case "PubKeyTxIn":
        case "SimpleScriptTxIn":
          return acc;
      }
    }, tx);

    // Step 3: Next, all the script outputs are added into the transaction, alongside their datums (if any).
    tx = outs.reduce((acc, { address, val, datum }) => {
      const targetAddr = addrToAddress(lucid.network, address);
      const targetVal = assetsFromAssetsP(val);
      return datum
        ? acc.payToAddressWithData(targetAddr, datum, targetVal)
        : acc.payToAddress(targetAddr, targetVal);
    }, tx);

    // Step 4: Next, all the minting information is added, alongside their redeemers.
    tx = Object.entries(mint).reduce(
      (acc, [assetUnit, { redeemer, amount }]) =>
        acc.mintAssets({ [assetUnit]: BigInt(amount) }, redeemer),
      tx,
    );

    // Step 5: Next, all the required signers are noted down.
    tx = requiredSignatories.reduce((acc, x) => acc.addSignerKey(x), tx);

    // Step 6: Next, the validators to be invoked are attached one by one.
    tx = inps.reduce(
      (
        acc,
        { details },
      ) => (details.tag == "ScriptTxIn"
        ? acc.attachSpendingValidator(details.validator)
        : acc),
      tx,
    );

    // Step 7: Next, the minting policies to be invoked are attached one by one.
    tx = Object.values(mint).reduce(
      (acc, { policy }) => acc.attachMintingPolicy(policy),
      tx,
    );

    // Step 8: The validity range of the transaction is set (if provided).
    tx = validityRange.validFrom == null
      ? tx
      : tx.validFrom(validityRange.validFrom);
    tx = validityRange.validTo == null ? tx : tx.validTo(validityRange.validTo);

    // Step 9: Finally, any extra datums are added to the transaction.
    tx = extraDatums.reduce((acc, x) => {
      acc.txBuilder.add_plutus_data(C.PlutusData.from_bytes(fromHex(x)));
      return acc;
    }, tx);

    return tx;
  };
}

// Below are a few utility functions that convert the Haskell returned types into Lucid native types.

function assetsFromAssetsP(asp: AssetsP): Assets {
  const res: Assets = {};
  for (const k in asp) {
    res[k] = BigInt(asp[k]);
  }
  return res;
}

function addrToAddress(network: Network, addr: Addr): Address {
  const networkId = networkToId(network);
  const primaryCoreCred = credentialToCore(addr.primary);
  switch (addr.tag) {
    case "Base":
      return C.BaseAddress.new(
        networkId,
        primaryCoreCred,
        credentialToCore(addr.stake),
      ).to_address().to_bech32();
    case "Enterprise":
      return C.EnterpriseAddress.new(networkId, primaryCoreCred).to_address()
        .to_bech32();
    case "Pointer":
      return C.PointerAddress.new(
        networkId,
        primaryCoreCred,
        pointerFrom(addr.stake),
      ).to_address().to_bech32();
  }
}

function credentialToCore(cred: Credential) {
  switch (cred.type) {
    case "Key":
      return C.StakeCredential.from_keyhash(
        C.Ed25519KeyHash.from_hex(cred.hash),
      );
    case "Script":
      return C.StakeCredential.from_scripthash(
        C.ScriptHash.from_hex(cred.hash),
      );
  }
}

function pointerFrom(
  { slot, txIdx, certIdx }: { slot: number; txIdx: number; certIdx: number },
) {
  const slotBig = C.BigNum.from_str(BigInt(slot).toString());
  const txIdxBig = C.BigNum.from_str(BigInt(txIdx).toString());
  const certIdxBig = C.BigNum.from_str(BigInt(certIdx).toString());
  return C.Pointer.new(slotBig, txIdxBig, certIdxBig);
}
