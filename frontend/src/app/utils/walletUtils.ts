//Axios imports
import axios, { AxiosResponse } from 'axios';

//Lucis imports
import { Address, Assets, Blockfrost, CML, credentialToAddress, credentialToRewardAddress, Lucid, LucidEvolution, makeTxSignBuilder, Network, paymentCredentialOf, scriptHashToCredential, toText, toUnit, TxSignBuilder, Unit, valueToAssets, walletFromSeed } from "@lucid-evolution/lucid";
import type { Credential as LucidCredential, ScriptHash } from "@lucid-evolution/core-types";
import { WalletBalance, DemoEnvironment } from '../store/types';

export async function makeLucid(demoEnvironment: DemoEnvironment) {
  const API_KEY_ENV = process.env.NEXT_PUBLIC_BLOCKFROST_API_KEY;
  const API_KEY = (API_KEY_ENV) ? API_KEY_ENV : demoEnvironment.blockfrost_key;

  const blockfrostURL = demoEnvironment.blockfrost_url;

  const blockfrost = new Blockfrost(blockfrostURL, API_KEY);

  const lucid = await Lucid(blockfrost, demoEnvironment.network);

  return lucid;
}

export async function getWalletFromSeed(demoEnvironment: DemoEnvironment, mnemonic: string) {
  try {
    const wallet = walletFromSeed(mnemonic, {password: '', addressType: 'Base', accountIndex: 0, network: demoEnvironment.network });
    return wallet;
  } catch (error) {
    console.error('Failed to initialize KeyAgent:', error);
    throw error; 
  }
}

export type UserBalanceResponse =
  { programmable_tokens: any
  , user_lovelace: number
  , ada_only_outputs: number
  }

export async function getWalletBalance(demoEnv: DemoEnvironment, address: string): Promise<WalletBalance> {
  try {
    const response = await axios.get<UserBalanceResponse>(
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
    if (response?.data && response.data.programmable_tokens[balance] && response.data.programmable_tokens[balance][stableTokenUnit]) {
      stableBalance = response.data.programmable_tokens[balance][stableTokenUnit];
    }

    return {wst: stableBalance, ada: response.data.user_lovelace / 1000000, adaOnlyOutputs: response.data.ada_only_outputs };
  } catch (error) {
    console.warn('Failed to get balance', error);
    return { wst: 0, ada: 0, adaOnlyOutputs: 0};
  }
}

export async function getBlacklist(address: string){
  try {
    const response = await axios.get(
      `/api/v1/query/blacklist/${address}`,  
      {
        headers: {
          'Content-Type': 'application/json;charset=utf-8', 
        },
      }
    );
    
    return response.data;
  } catch (error) {
      console.warn('Failed to get blacklist', error);
      return error;
  }
}

export async function getProgrammableTokenAddress(address: string): Promise<string> {
  const response = await axios.get<string>(`/api/v1/query/address/${address}`,
    {
      headers: {
        'Content-Type': 'application/json;charset=utf-8',
      },
    });
  return response?.data;
}

export async function getFreezePolicyId(address: string): Promise<string> {
  const response = await axios.get<string>(`/api/v1/query/freeze-policy-id/${address}`,
    {
      headers: {
        'Content-Type': 'application/json;charset=utf-8',
      },
    });
  return response?.data;
}

export async function getPolicyIssuer(policyId: string): Promise<string> {
  const response = await axios.get<string>(`/api/v1/query/policy-issuer/${policyId}`, {
    headers: {
      'Content-Type': 'application/json;charset=utf-8',
    },
  });
  return response?.data;
}

const getBlockfrostProjectId = (demoEnv: DemoEnvironment) =>
  process.env.NEXT_PUBLIC_BLOCKFROST_API_KEY ?? demoEnv.blockfrost_key;

const trimTrailingSlash = (url: string) => url.replace(/\/+$/, '');

export async function getStakeScriptHashes(address: string): Promise<ScriptHash[]> {
  try {
    const response = await axios.get<ScriptHash[]>(`/api/v1/query/stake-scripts/${address}`, {
      headers: {
        'Content-Type': 'application/json;charset=utf-8',
      },
    });
    return response?.data ?? [];
  } catch (error) {
    console.warn('Failed to fetch stake script hashes', error);
    return [];
  }
}

const buildAccountsEndpoint = (baseUrl: string, rewardAddress: string) => {
  const trimmed = trimTrailingSlash(baseUrl);
  return `${trimmed}/accounts/${rewardAddress}`;
};

const stakeScriptActive = async (demoEnv: DemoEnvironment, rewardAddress: string): Promise<boolean> => {
  try {
    const response = await axios.get(
      buildAccountsEndpoint(demoEnv.blockfrost_url, rewardAddress),
      {
        headers: {
          project_id: getBlockfrostProjectId(demoEnv),
        },
      }
    );
    return response?.data?.active_epoch && response?.data?.active_epoch > 0;
  } catch (error) {
    console.warn(`Failed to check stake script status for ${rewardAddress}`, error);
    return false;
  }
};

export async function areStakeScriptsRegistered(
  demoEnv: DemoEnvironment,
  lucid: LucidEvolution,
  issuerAddress: string
): Promise<boolean> {
  try {
    if (!lucid || typeof lucid.config !== 'function') {
      console.warn('Lucid is not initialised; cannot verify stake script registration.');
      return false;
    }
    const network = lucid.config().network ?? demoEnv.network;
    const scriptHashes = await getStakeScriptHashes(issuerAddress);
    if (scriptHashes.length === 0) {
      return false;
    }
    const statuses = await Promise.all(
      scriptHashes.map(async (hash) => {
        const rewardAddress = deriveStakeAddressFromScriptHash(hash, lucid, network);
        return stakeScriptActive(demoEnv, rewardAddress);
      })
    );
    return statuses.every(Boolean);
  } catch (error) {
    console.warn('Failed to determine stake script registration status', error);
    return false;
  }
}

export type PolicyHolder = {
  address: string;
  assets: Array<{ unit: string; quantity: string; assetName?: string }>;
};

export const decodeAssetName = (hexName?: string): string | undefined => {
  if (!hexName) {
    return undefined;
  }
  try {
    return toText(hexName);
  } catch (error) {
    console.warn('Failed to decode asset name from hex', hexName, error);
    return hexName;
  }
};

type UserProgrammableValueResponse = Record<string, number | Record<string, number | string>>;

const normaliseQuantity = (value: number | string | undefined): string => {
  if (typeof value === 'number') {
    return value.toString();
  }
  if (typeof value === 'string' && value.trim().length > 0) {
    return value;
  }
  return '0';
};

const isAssetMap = (value: unknown): value is Record<string, number | string> =>
  typeof value === 'object' && value !== null && !Array.isArray(value);

export type PolicyTokenBalance = {
  policyId: string;
  tokens: Array<{ assetNameHex: string; displayName: string; quantity: string }>;
};

const parseProgrammableValue = (data: UserProgrammableValueResponse): PolicyTokenBalance[] => {
  return Object.entries(data ?? {})
    .filter(([policyId, assets]) => policyId !== 'lovelace' && isAssetMap(assets))
    .map(([policyId, assets]) => {
      const tokens = Object.entries(assets as Record<string, number | string>).map(([assetHex, quantity]) => {
        const displayName =
          decodeAssetName(assetHex) ??
          (assetHex && assetHex.length > 0 ? assetHex : 'Unnamed asset');
        return {
          assetNameHex: assetHex,
          displayName,
          quantity: normaliseQuantity(quantity),
        };
      });
      return { policyId, tokens };
    })
    .filter((group) => group.tokens.length > 0);
};

export async function getUserTotalProgrammableValue(address: string): Promise<PolicyTokenBalance[]> {
  if (!address) {
    return [];
  }
  try {
    const response = await axios.get<UserProgrammableValueResponse>(
      `/api/v1/query/user-total-programmable-value/${address}`,
      {
        headers: {
          'Content-Type': 'application/json;charset=utf-8',
        },
      }
    );
    return parseProgrammableValue(response?.data ?? {});
  } catch (error) {
    console.warn('Failed to fetch user programmable value', error);
    throw error;
  }
}

const extractAssetNameHex = (assetUnit: string, policyId: string): string | undefined => {
  if (!assetUnit?.startsWith(policyId)) {
    return undefined;
  }
  return assetUnit.slice(policyId.length);
};

const fetchPolicyAssets = async (demoEnv: DemoEnvironment, policyId: string) => {
  const projectId = getBlockfrostProjectId(demoEnv);
  const baseUrl = trimTrailingSlash(demoEnv.blockfrost_url);
  const assets: Array<{ asset: string; assetName?: string }> = [];
  const pageSize = 100;
  for (let page = 1; page <= 10; page++) {
    try {
      const { data } = await axios.get(
        `${baseUrl}/assets/policy/${policyId}`,
        {
          params: { page, count: pageSize },
          headers: { project_id: projectId },
        }
      );
      if (!Array.isArray(data) || data.length === 0) {
        break;
      }
      assets.push(
        ...data.map((entry: any) => ({
          asset: entry.asset,
          assetName:
            decodeAssetName(entry.asset_name) ??
            decodeAssetName(extractAssetNameHex(entry.asset, policyId)),
        }))
      );
      if (data.length < pageSize) {
        break;
      }
    } catch (error) {
      console.warn('Failed to fetch assets for policy', policyId, error);
      break;
    }
  }
  return assets;
};

export async function fetchPolicyHolders(
  demoEnv: DemoEnvironment,
  policyId: string
): Promise<PolicyHolder[]> {
  const projectId = getBlockfrostProjectId(demoEnv);
  const baseUrl = trimTrailingSlash(demoEnv.blockfrost_url);
  const assets = await fetchPolicyAssets(demoEnv, policyId);
  const holders = new Map<string, Array<{ unit: string; quantity: string; assetName?: string }>>();

  await Promise.all(
    assets.map(async ({ asset, assetName }) => {
      try {
        const { data } = await axios.get(
          `${baseUrl}/assets/${asset}/addresses`,
          { headers: { project_id: projectId } }
        );
        if (Array.isArray(data)) {
          data.forEach((entry: { address: string; quantity: string }) => {
            const current = holders.get(entry.address) ?? [];
            current.push({ unit: asset, quantity: entry.quantity, assetName });
            holders.set(entry.address, current);
          });
        }
      } catch (error) {
        console.warn('Failed to fetch holders for asset', asset, error);
      }
    })
  );

  return Array.from(holders.entries()).map(([address, assetsList]) => ({
    address,
    assets: assetsList,
  }));
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
  const redeemers = witnessSet.redeemers();
  const languages = witnessSet.languages();
  const expectedScriptDataHash : CML.ScriptDataHash | undefined =
    redeemers && languages && languages.len() > 0
      ? CML.calc_script_data_hash(redeemers, CML.PlutusDataList.new(), lucid.config().costModels!, languages)
      : undefined;
  const cmlTxBodyClone = CML.TransactionBody.from_cbor_hex(cmlTx!.body().to_cbor_hex());
  if (expectedScriptDataHash) {
    cmlTxBodyClone.set_script_data_hash(expectedScriptDataHash);
  }
  
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
  console.log("Network", network, demoEnv.network);
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

export function deriveStakeAddressFromScriptHash(stakeScriptHash: ScriptHash, lucid: LucidEvolution, networkOverride?: Network): string{
  const network = networkOverride ?? lucid.config().network!;
  // user's staking script credentials
  const stakeCredential = scriptHashToCredential(stakeScriptHash);
  return credentialToRewardAddress(network, stakeCredential);
}
