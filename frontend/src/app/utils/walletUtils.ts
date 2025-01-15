//Axios imports
import axios from 'axios';

//Lucis imports
import { Blockfrost, CML, Lucid, LucidEvolution, TxSigned, walletFromSeed } from "@lucid-evolution/lucid";

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

export async function getWalletBalance(address: string){
  try {
    const response = await axios.get(
      `/api/v1/query/user-funds/${address}`,  
      {
        headers: {
          'Content-Type': 'application/json;charset=utf-8', 
        },
      }
    );
    const balance = "b34a184f1f2871aa4d33544caecefef5242025f45c3fa5213d7662a9";
    const stableTokenUnit = "575354"; 
    let stableBalance = 0;
    if (response?.data && response.data[balance] && response.data[balance][stableTokenUnit]) {
      stableBalance = response.data[balance][stableTokenUnit];
    }
    // console.log('Get wallet balance:', response.data);
    return stableBalance;
  } catch (error) {
    console.error('Failed to get balance', error);
    return 0;
  }
}

export function setScriptDataHash(tx: CML.Transaction, newScriptDataHash: CML.ScriptDataHash) : CML.Transaction {
  const txBodyClone = CML.TransactionBody.from_cbor_hex(tx.body().to_cbor_hex());
  txBodyClone.set_script_data_hash(newScriptDataHash);
  const clonedTx = CML.Transaction.new(txBodyClone, tx.witness_set(), true, tx.auxiliary_data());
  tx.free();
  return clonedTx;
}

export function adjustScriptDataHash(tx: TxSigned, costModel: CML.CostModels) : CML.Transaction {
  const cmlTx = tx.toTransaction();
  const witnessSet = cmlTx.witness_set()
  const plutusDatumsI = witnessSet.plutus_datums()
  const plutusDatums : CML.PlutusDataList = (plutusDatumsI !== undefined) ? plutusDatumsI : CML.PlutusDataList.new();
  const expectedScriptDataHash : CML.ScriptDataHash | undefined = CML.calc_script_data_hash(witnessSet.redeemers()!, plutusDatums, costModel, witnessSet.languages());
  const fixedTx = setScriptDataHash(cmlTx, expectedScriptDataHash!);
  return fixedTx
}

export type WalletType = "Lace" | "Eternl" | "Nami" | "Yoroi";

export async function selectLucidWallet(lucid: LucidEvolution, wallet: WalletType) {
  const api = (await window.cardano[wallet.toLowerCase()].enable());
  lucid.selectWallet.fromAPI(api);
}