import {
  Blockfrost,
  Lucid,
  LucidEvolution,
  walletFromSeed,
} from "@lucid-evolution/lucid";

export async function makeLucid() {
        const API_KEY = process.env.NEXT_PUBLIC_BLOCKFROST_API_KEY;

        if (!API_KEY) {
            throw new Error(
                "Missing required environment variables for Blockfrost context.",
            );
        }

        let blockfrostURL = "https://cardano-preview.blockfrost.io/api/v0";
        const blockfrost = new Blockfrost(blockfrostURL, API_KEY);

        const lucid = await Lucid(blockfrost, "Preview");

        return lucid;
}

export async function getWalletFromSeed(mnemonic: string) {
  try {
    let mintWallet = walletFromSeed(mnemonic, {password: '', addressType: 'Base', accountIndex: 0, network: "Preview"});
    return mintWallet;
  } catch (error) {
    console.error('Failed to initialize KeyAgent:', error);
    throw error; 
  }
}

// export async function changeCurrentLucidWallet(lucid: LucidEvolution) {
//   try {
//     lucid.
//   } catch (error) {

//   }
// }

export type WalletType = "Lace" | "Eternl" | "Nami" | "Yoroi";

export async function selectLucidWallet(lucid: LucidEvolution, wallet: WalletType) {
  const api = (await window.cardano[wallet.toLowerCase()].enable());
  lucid.selectWallet.fromAPI(api);
}