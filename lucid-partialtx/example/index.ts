import { Blockfrost, Lucid } from 'lucid-cardano';
import { PartialTx } from '..';
import * as LucidPartialTx from '..';

import { blockfrostUrl, blockfrostProjId } from './config.json';

// Initialise the blockfrost instance.
const blockfrostInst = new Blockfrost(blockfrostUrl, blockfrostProjId);

// Initialise the lucid instance.
const lucid = await Lucid.new(blockfrostInst, 'Testnet');

// Assumes you are in a browser environment - enables interfacing with Nami.
// @ts-ignore
lucid.selectWallet(await window.cardano.nami.enable());

const mintABtn = document.getElementById('mintA')!;

// JSON representation of Haskell unit i.e `()`.
export const unitJson = JSON.stringify([]);

const originUrl = new URL(window.location.href).origin;

const ownWalletAddress = await lucid.wallet.address();
const ownPkh = lucid.utils.getAddressDetails(ownWalletAddress).paymentCredential!.hash;
const ownSkh = lucid.utils.getAddressDetails(ownWalletAddress).stakeCredential?.hash;

const buildTxFrom = LucidPartialTx.buildTxFrom.bind(lucid);

mintABtn.onclick = async () => {
    const res = await fetch(`${originUrl}/contracts/dummyMintA`, {
        method: 'POST',
        headers: {
          OwnPubKeyHash: ownPkh,
          'Content-Type': 'application/json',
          ...(ownSkh ? { OwnStakePubKeyHash: ownSkh } : {}),
        },
        body: unitJson,
    });
    if (!res.ok) {
        alert('Failure - check console');
        return;
    }
    const partialTx: PartialTx = await res.json();
    console.log(partialTx);

    const tx = await buildTxFrom(partialTx).complete();
    const signedTx = await tx.sign().complete();
    const txHash = await signedTx.submit();

    console.log(txHash);
};
