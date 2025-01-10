import { InMemoryKeyAgent } from '@cardano-sdk/key-management';
import { Cardano } from '@cardano-sdk/core';
import { dummyLogger } from 'ts-log';
import * as Crypto from '@cardano-sdk/crypto';
import * as bip39 from 'bip39';
import {
  Blockfrost,
  Lucid,
  LucidEvolution,
} from "@lucid-evolution/lucid";

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

// Function to create a new wallet
export async function createNewWallet() {
    const bip32Ed25519 = new Crypto.SodiumBip32Ed25519();
    const mnemonicToWords = (mnemonic: string) => mnemonic.split(' ');
    const generateMnemonicWords = (strength = 256) => mnemonicToWords(bip39.generateMnemonic(strength));
  
    try {
      // Generate a new mnemonic phrase
      const mnemonic = generateMnemonicWords();
  
      // Create a new KeyAgent
      const agent = await InMemoryKeyAgent.fromBip39MnemonicWords(
        {
          chainId: Cardano.ChainIds.Preview, // Replace with your desired chain ID
          getPassphrase: async () => Buffer.from(''), // Replace with a secure passphrase
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

export type Network = "Mainnet" | "Preprod" | "Preview" | "Custom";

export const NETWORK = (process.env.NETWORK as Network) || "Preview";

export async function makeLucid(network: Network) {
        const API_KEY = "previewzwnjcGmHgYLFmLppEWCrmbhapNtCq4H7";

        if (!API_KEY) {
            throw new Error(
                "Missing required environment variables for Blockfrost context.",
            );
        }

        if (network === "Custom") {
            throw new Error(
                "Cannot create Blockfrost context with 'Custom' network.",
            );
        }

        // https://cardano-preprod.blockfrost.io/api/v0/
        let blockfrostURL = "https://cardano-" + network.toLowerCase() + ".blockfrost.io/api/v0/";
        const blockfrost = new Blockfrost(blockfrostURL, API_KEY);

        const lucid = await Lucid(blockfrost, network);

        return lucid;
}

export type WalletType = "Lace" | "Eternl" | "Nami" | "Yoroi";

export const selectWallet = async (lucid : LucidEvolution, wallet : WalletType) => {
  const api = (await window.cardano[wallet.toLowerCase()].enable());
  lucid.selectWallet.fromAPI(api);
}