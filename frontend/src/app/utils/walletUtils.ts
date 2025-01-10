import axios from 'axios';
import { InMemoryKeyAgent } from '@cardano-sdk/key-management';
import { Cardano, Serialization } from '@cardano-sdk/core';
import { dummyLogger } from 'ts-log';
import * as Crypto from '@cardano-sdk/crypto';
import * as bip39 from 'bip39';

//Lucid imports
//import { Lucid, Blockfrost } from "@lucid-evolution/lucid";

//sdk option
export async function initializeMintWallet(mnemonic: string[]) {
  const bip32Ed25519 = new Crypto.SodiumBip32Ed25519();

  try {
    const agent = await InMemoryKeyAgent.fromBip39MnemonicWords(
      {
        chainId: Cardano.ChainIds.Preview,
        getPassphrase: async () => Buffer.from(''),
        mnemonicWords: mnemonic,
      },
      { bip32Ed25519, logger: dummyLogger }
    );
    return agent;
  } catch (error) {
    console.error('Failed to initialize KeyAgent:', error);
    throw error; 
  }
}

//Lucid option
// export async function lucidGenerateSeedWallet(): Promise<string> {
//   try {
//     const lucid = await Lucid(
//       new Blockfrost(
//         "https://cardano-preview.blockfrost.io/api/v0",
//         "previewYl4KaKw8ZMiHP11GBXqpkBU15DHYwP0E"
//       ),
//       "Preview"
//     );

//     const seedPhrase =
//       "problem alert infant glance toss gospel tonight sheriff match else hover upset chicken desert anxiety cliff moment song large seed purpose chalk loan onion";

//     lucid.selectWallet.fromSeed(seedPhrase);
//     const address = await lucid.wallet().address();

//     if (!address) {
//       throw new Error("Failed to retrieve wallet address.");
//     }

//     console.log("Lucid KeyAgent initialized:", address);
//     return address;
//   } catch (error) {
//     console.error("Error generating Lucid wallet:", error);
//     throw error; 
//   }
// }

export async function createNewWallet() {
    const bip32Ed25519 = new Crypto.SodiumBip32Ed25519();
    const mnemonicToWords = (mnemonic: string) => mnemonic.split(' ');
    const generateMnemonicWords = (strength = 256) => mnemonicToWords(bip39.generateMnemonic(strength));
  
    try {
      const mnemonic = generateMnemonicWords();
  
      const agent = await InMemoryKeyAgent.fromBip39MnemonicWords(
        {
          chainId: Cardano.ChainIds.Preview, 
          getPassphrase: async () => Buffer.from(''), 
          mnemonicWords: mnemonic,
        },
        { bip32Ed25519, logger: dummyLogger }
      );
  
      // Derive the first address
      const address = await agent.deriveAddress({ index: 0, type: 0 }, 0);
  
      return {
        agent,
        address: address.address, 
        mnemonic,
      };
    } catch (error) {
      console.error('Failed to create a new wallet:', error);
      throw error; 
    }
  }

  export async function signTX(cborHex
    : string, keyAgent: InMemoryKeyAgent | undefined) {
    try {
      if(keyAgent === undefined) throw new Error('KeyAgent not initialized.');
      // Deserialize the transaction
      const tx = Serialization.Transaction.fromCbor(Serialization.TxCBOR(cborHex));
      const body = tx.body(); 
      const coreBody = body.toCore();
      const txHash = tx.getId(); 

      // console.log('Transaction Body:', body);
      // console.log('Transaction coreBody:', coreBody);
      // console.log('Transaction Hash:', txHash);

      const txBodyWithHash = {
        body: coreBody,
        hash: txHash, 
      };

      const address = await keyAgent.deriveAddress({ index: 0, type: 0 }, 0);

      // Sign the transaction
      const witnessSet = await keyAgent.signTransaction(txBodyWithHash, {
        knownAddresses: [address],
        txInKeyPathMap: {},
      });

      // Attach signatures to the transaction's witness set
      const mintTxCore = Serialization.TxCBOR.deserialize(Serialization.TxCBOR(cborHex));
      mintTxCore.witness.signatures = witnessSet;

      // Create a signed transaction
      const mintTxSigned = Serialization.Transaction.fromCore(mintTxCore);
      const mintTxSignedCbor = mintTxSigned.toCbor();

      // console.log('Witness Signatures:', witnessSet);
      // console.log('Witness Set:', mintTxCore.witness);
      
      // Re-compute and compare the transaction hash
      // const recomputedHash = mintTxSigned.body().hash();

      submitTransactionToBlockfrost(mintTxSignedCbor, txHash);
    } catch (error) {
      console.error('Failed to sign the transaction:', error);
      throw error; 
    }
  }

  export async function submitTransactionToBlockfrost(signedTxCbor: string, txHash: Cardano.TransactionId): Promise<string> {
    try {
      const blockfrostUrl = 'https://cardano-mainnet.blockfrost.io/api/v0/tx/submit';
      const projectId = 'preview2Qp4Bdm7nsoaW8h43NURAYgsE14ikZ0x'; 

      const response = await axios.post(
        blockfrostUrl,
        signedTxCbor, 
        {
          headers: {
            'Content-Type': 'application/cbor',
            project_id: projectId,
          },
        }
      );
  
      console.log('Transaction submitted successfully:', response.data);
      return response.data; // Contains the transaction ID
    } catch (error: any) {
      console.error('Failed to submit transaction:', error.response?.data || error.message);
      throw error;
    }
  }