//Axios imports
import axios from 'axios';

//Lucis imports
import { Blockfrost, CML, Lucid, LucidEvolution, TxSigned, walletFromSeed, Credential, valueToAssets, Assets, UTxO, Address, paymentCredentialOf, credentialToAddress, toUnit, Unit } from "@lucid-evolution/lucid";

async function loadKey() {
  const response = await axios.get("/blockfrost-key",
    {
      headers: {
        'Content-Type': 'application/json;charset=utf-8', 
      },
    });
  const BE_KEY = response?.data;
  return BE_KEY;
}

export async function makeLucid() {
        const API_KEY_ENV = process.env.NEXT_PUBLIC_BLOCKFROST_API_KEY;
        const API_KEY = (API_KEY_ENV) ? API_KEY_ENV : await loadKey();

        const blockfrostURL = "https://cardano-preview.blockfrost.io/api/v0"

        const blockfrost = new Blockfrost(blockfrostURL, API_KEY);

        const lucid = await Lucid(blockfrost, "Preview");

        return lucid;
}

export async function getWalletFromSeed(mnemonic: string) {
  try {
    const mintWallet = walletFromSeed(mnemonic, {password: '', addressType: 'Base', accountIndex: 0, network: "Preview"});
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

const progLogicBase : Credential = {
  type: "Script",
  hash: "fca77bcce1e5e73c97a0bfa8c90f7cd2faff6fd6ed5b6fec1c04eefa"
}

const stableCoin : Unit = toUnit("b34a184f1f2871aa4d33544caecefef5242025f45c3fa5213d7662a9", "575354")

export function adjustMintOutput(tx: CML.Transaction, receiverAddress: Address, mintedAmount: bigint) {
  const txB : CML.TransactionBody = tx.body()
  const new_outputs = CML.TransactionOutputList.new()

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

export async function getStablecoinAccounts(lucid: LucidEvolution) {
  const progUTxOs : UTxO[] = await lucid.utxosAtWithUnit(progLogicBase, stableCoin);
  const addresses = new Set<string>();
  const valueMap = new Map<Address, number>();
  progUTxOs.forEach(utxo => {
    addresses.add(utxo.address)
    valueMap.set(utxo.address, Number(utxo.assets[stableCoin]))
  });
  return { addresses: Array.from(addresses), valueMap };
}

export async function deriveProgrammableAddress(lucid: LucidEvolution, userAddress: Address){
  const network = lucid.config().network!;
  // user's payment credential
  const ownerCred : Credential = paymentCredentialOf(userAddress);

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