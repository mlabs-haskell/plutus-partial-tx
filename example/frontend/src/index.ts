import { Blockfrost, Lucid } from "lucid-cardano";
import { PartialTx, mkPartialTxInterpreter } from "lucid-cardano-partialtx";

import { blockfrostProjId, blockfrostUrl } from "../config.json";

type WalletPropname = 'nami' | 'eternl';
type WalletObjContainer = Record<WalletPropname, unknown>;

function getWalletObj(x: WalletObjContainer) {
  if ('nami' in x) {
    return x.nami;
  } else if ('eternl' in x) {
    return x.eternl;
  }
}

// Initialise the blockfrost instance.
const blockfrostInst = new Blockfrost(blockfrostUrl, blockfrostProjId);

// Initialise the lucid instance.
const lucid = await Lucid.new(blockfrostInst, "Testnet");

// Assumes you are in a browser environment - enables interfacing with Nami.
// @ts-ignore
lucid.selectWallet(await getWalletObj(window.cardano).enable());

const mintABtn = document.getElementById("mintA")!;

// JSON representation of Haskell unit i.e `()`.
export const unitJson = JSON.stringify([]);

const originUrl = new URL(window.location.href).origin;

const ownWalletAddress = await lucid.wallet.address();
const ownPkh = lucid.utils.getAddressDetails(ownWalletAddress).paymentCredential!.hash;
const ownSkh = lucid.utils.getAddressDetails(ownWalletAddress).stakeCredential?.hash;

const buildTxFrom = mkPartialTxInterpreter(lucid);

mintABtn.onclick = async () => {
  const res = await fetch(`${originUrl}/contracts/dummyMintA`, {
    method: "POST",
    headers: {
      OwnPubKeyHash: ownPkh,
      "Content-Type": "application/json",
      ...(ownSkh ? { OwnStakePubKeyHash: ownSkh } : {}),
    },
    body: unitJson,
  });
  if (!res.ok) {
    alert("Failure - check console");
    return;
  }
  const partialTx: PartialTx = await res.json();
  console.log(partialTx);

  const tx = await buildTxFrom(partialTx).complete();
  const signedTx = await tx.sign().complete();
  const txHash = await signedTx.submit();

  console.log(txHash);
};
