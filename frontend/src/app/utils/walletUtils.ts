//Axios imports
import axios, { AxiosResponse } from 'axios';

//Lucis imports
import { Address, Assets, Blockfrost, CML, credentialToAddress, Lucid, LucidEvolution, makeTxSignBuilder, paymentCredentialOf, toUnit, TxSignBuilder, Unit, valueToAssets, walletFromSeed } from "@lucid-evolution/lucid";
import type { Credential as LucidCredential } from "@lucid-evolution/core-types";
import { WalletBalance, DemoEnvironment } from '../store/types';

export async function makeLucid(demoEnvironment: DemoEnvironment) {
  const API_KEY_ENV = process.env.NEXT_PUBLIC_BLOCKFROST_API_KEY;
  const API_KEY = (API_KEY_ENV) ? API_KEY_ENV : demoEnvironment.blockfrost_key;

  const blockfrostURL = demoEnvironment.blockfrost_url;

  const blockfrost = new Blockfrost(blockfrostURL, API_KEY);

  const lucid = await Lucid(blockfrost, demoEnvironment.network);

  return lucid;
}

export async function getWalletFromSeed(mnemonic: string) {
  try {
    const wallet = walletFromSeed(mnemonic, {password: '', addressType: 'Base', accountIndex: 0, network: "Preview"});
    return wallet;
  } catch (error) {
    console.error('Failed to initialize KeyAgent:', error);
    throw error; 
  }
}

export async function getWalletBalance(demoEnv: DemoEnvironment, address: string): Promise<WalletBalance> {
  try {
    const response = await axios.get(
      `/api/v1/query/user-funds/${address}`,  
      {
        headers: {
          'Content-Type': 'application/json;charset=utf-8', 
        },
      }
    );
    const balance = demoEnv.minting_policy;
    const stableTokenUnit = demoEnv.token_name; 
    let stableBalance = 0;
    let adaBalance = 0;
    if (response?.data && response.data[balance] && response.data[balance][stableTokenUnit]) {
      stableBalance = response.data[balance][stableTokenUnit];
      adaBalance = response.data["lovelace"] / 1000000;
    }

    return {wst: stableBalance, ada: adaBalance };
  } catch (error) {
    console.error('Failed to get balance', error);
    return { wst: 0, ada: 0};
  }
}

export async function getBlacklist(demoEnv: DemoEnvironment){
  try {
    const response = await axios.get(
      `/api/v1/query/blacklist/${demoEnv.transfer_logic_address}`,  
      {
        headers: {
          'Content-Type': 'application/json;charset=utf-8', 
        },
      }
    );
    
    // console.log('Get blacklist:', response);
    return response.data;
  } catch (error) {
      console.warn('Failed to get blacklist', error);
      return error;
  }
}

export async function submitTx(tx: string): Promise<AxiosResponse<any, any>> {
  return axios.post(
      '/api/v1/tx/submit',
      {
        description: "",
        type: "Tx ConwayEra",
        cborHex: tx
      },
      {
        headers: {
          'Content-Type': 'application/json;charset=utf-8', 
        },
      }
    );
  }

export async function signAndSentTx(lucid: LucidEvolution, tx: TxSignBuilder): Promise<string> {
  const isValid = tx.toTransaction().is_valid();
  const txBuilder = await makeTxSignBuilder(lucid.wallet(), tx.toTransaction()).complete();
  const cmlTx = txBuilder.toTransaction();
  const witnessSet = txBuilder.toTransaction().witness_set();
  const expectedScriptDataHash : CML.ScriptDataHash | undefined = CML.calc_script_data_hash(witnessSet.redeemers()!, CML.PlutusDataList.new(), lucid.config().costModels!, witnessSet.languages());
  const cmlTxBodyClone = CML.TransactionBody.from_cbor_hex(cmlTx!.body().to_cbor_hex());
  cmlTxBodyClone.set_script_data_hash(expectedScriptDataHash!);
  
  const cmlClonedTx = CML.Transaction.new(cmlTxBodyClone, cmlTx!.witness_set(), isValid, cmlTx!.auxiliary_data());
  const cmlClonedSignedTx = await makeTxSignBuilder(lucid.wallet(), cmlClonedTx).sign.withWallet().complete();

  // We need to reconstruct the transaction from CBOR again, using CML, because the 'cmlClonedSignedTx.toTransaction().is_valid' always
  // returns true (overriding what we specified when we created cmlClonedTx)
  const clonedTx2 = CML.Transaction.new(CML.TransactionBody.from_cbor_hex(cmlClonedSignedTx.toTransaction().body().to_cbor_hex()), cmlClonedSignedTx!.toTransaction().witness_set(), isValid, cmlClonedSignedTx.toTransaction()!.auxiliary_data());
  const txId = await submitTx(clonedTx2.to_cbor_hex());
  const i = txId.data;
  console.log(txId);
  await lucid.awaitTx(txId.data);
  return i
}

export type WalletType = "Lace" | "Eternl" | "Nami" | "Yoroi";

export async function selectLucidWallet(lucid: LucidEvolution, wallet: WalletType) {
  const api = (await window.cardano[wallet.toLowerCase()].enable());
  lucid.selectWallet.fromAPI(api);
}


export function adjustMintOutput(demoEnv: DemoEnvironment, tx: CML.Transaction, receiverAddress: Address, mintedAmount: bigint) {
  const txB : CML.TransactionBody = tx.body()
  const new_outputs = CML.TransactionOutputList.new()
  const stableCoin : Unit = toUnit(demoEnv.minting_policy, demoEnv.token_name);
  
  const outputs : CML.TransactionOutputList = txB.outputs()
  const outputsLen = outputs.len()
  for (let i = 0; i < outputsLen; i++) {
    const output : CML.TransactionOutput = outputs.get(i)
    const assets : Assets = valueToAssets(output.amount())
    if (stableCoin in assets) {
      console.log("Found stablecoin in output")
      const stablecoinAmount = assets[stableCoin]
      if (stablecoinAmount == mintedAmount){
        console.log("Found minted amount in output")
        const newOutput = CML.TransactionOutput.new(CML.Address.from_bech32(receiverAddress), output.amount(), output.datum(), output.script_ref())
        new_outputs.add(newOutput)
        //new(address: Address, amount: Value, datum_option?: DatumOption, script_reference?: Script): TransactionOutput;
      } else {
        new_outputs.add(output)
      }
    } else {
      new_outputs.add(output)
    }
    // if (value !== undefined) {
    //   const newValue = value + 100
    //   assets.insert(stableCoin, newValue)
    // }
  }
  const newTxB : CML.TransactionBody = CML.TransactionBody.new(txB.inputs(), new_outputs, txB.fee());
  const oldTxAuxHash = txB.auxiliary_data_hash()
  if(oldTxAuxHash){
    newTxB.set_auxiliary_data_hash(oldTxAuxHash)
  }
  const oldWithdrawals = txB.withdrawals()
  if(oldWithdrawals){
    newTxB.set_withdrawals(oldWithdrawals)
  }
  const oldTTL = txB.ttl()
  if(oldTTL){
    newTxB.set_ttl(oldTTL)
  }
  const oldCerts = txB.certs()
  if(oldCerts){
    newTxB.set_certs(oldCerts)
  }

  const oldValidityStart = txB.validity_interval_start()
  if(oldValidityStart){
    newTxB.set_validity_interval_start(oldValidityStart)
  }

  const oldMint = txB.mint()
  if(oldMint){
    newTxB.set_mint(oldMint)
  }

  const oldCollateral = txB.collateral_inputs()
  if(oldCollateral){
    newTxB.set_collateral_inputs(oldCollateral)
  }

  const oldRequiredSigners = txB.required_signers()
  if(oldRequiredSigners){
    newTxB.set_required_signers(oldRequiredSigners)
  }

  const oldNetworkId = txB.network_id()
  if(oldNetworkId){
    newTxB.set_network_id(oldNetworkId)
  }

  const oldCollateralReturn = txB.collateral_return()
  if(oldCollateralReturn){
    newTxB.set_collateral_return(oldCollateralReturn)
  }

  const oldTotalCollateral = txB.total_collateral()
  if(oldTotalCollateral){
    newTxB.set_total_collateral(oldTotalCollateral)
  }

  const oldReferenceInputs = txB.reference_inputs();
  if(oldReferenceInputs){
    newTxB.set_reference_inputs(oldReferenceInputs)
  }

  const oldTreasuryValue = txB.current_treasury_value()
  if(oldTreasuryValue){
    newTxB.set_current_treasury_value(oldTreasuryValue)
  }

  console.log("New outputs length: ", new_outputs.len())  
  // const oldAuxiliaryData = tx.auxiliary_data()
  // if(oldAuxiliaryData){
  
  // }
  return CML.Transaction.new(newTxB, tx.witness_set(), true, tx.auxiliary_data())

}

export async function deriveProgrammableAddress(demoEnv: DemoEnvironment, lucid: LucidEvolution, userAddress: Address){
  const network = lucid.config().network!;
  // user's payment credential
  const ownerCred : LucidCredential = paymentCredentialOf(userAddress);

  const progLogicBase : LucidCredential = {
    type: "Script",
    hash: demoEnv.prog_logic_base_hash
  }
  // construct the user's programmable token address
  // payment credential is always the programmable token base script hash
  // staking credential is the user's payment credential        
  const userProgrammableTokenAddress = credentialToAddress(
        network,
        progLogicBase,
        ownerCred,
      );
  return userProgrammableTokenAddress;

}