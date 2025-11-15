'use client';
//React imports
import React, { useContext, useEffect, useState, useMemo } from 'react';

//Axios imports
import axios from 'axios';

//Mui imports
import { Box, Checkbox, CircularProgress, FormControl, FormControlLabel, InputLabel, MenuItem, Paper, Select, Typography } from '@mui/material';
import type { SelectChangeEvent } from '@mui/material/Select';

//Local components
import useStore from '../store/store'; 
import { Accounts } from '../store/types';
import { getWalletBalance, signAndSentTx, getProgrammableTokenAddress, areStakeScriptsRegistered, getFreezePolicyId, fetchPolicyHolders, deriveProgrammableAddress, getUserTotalProgrammableValue, PolicyTokenBalance, getPolicyIssuer } from '../utils/walletUtils';
import WalletCard from '../components/Card';
import WSTTextField from '../components/WSTTextField';
import CopyTextField from '../components/CopyTextField';
import WSTTable, { WSTTableRow } from '../components/WSTTable';
import DemoEnvironmentContext from '../context/demoEnvironmentContext';

export default function Profile() {
  const { lucid, currentUser, mintAccount, changeAlertInfo, changeWalletAccountDetails, selectedTab } = useStore();
  const accounts = useStore((state) => state.accounts);
  const [overrideTx, setOverrideTx] = useState<boolean>(false);
  const [isRegistering, setIsRegistering] = useState<boolean>(false);
  const [isInitializingBlacklist, setIsInitializingBlacklist] = useState<boolean>(false);
  const [userAssetName, setUserAssetName] = useState('WST');
  const [userMintAmount, setUserMintAmount] = useState(0);
  const [userMintRecipient, setUserMintRecipient] = useState('');
  const [userAddressCleared, setUserAddressCleared] = useState(false);
  const [userFreezeAddress, setUserFreezeAddress] = useState('');
  const [userFreezeReason, setUserFreezeReason] = useState('Enter reason here');
  const [userUnfreezeAddress, setUserUnfreezeAddress] = useState('');
  const [userSeizeAddress, setUserSeizeAddress] = useState('');
  const [userSeizeReason, setUserSeizeReason] = useState('Enter reason here');
  const [policyOptions, setPolicyOptions] = useState<Array<{ id: string; label: string }>>([]);
  const [selectedPolicy, setSelectedPolicy] = useState('');
  const [policyRows, setPolicyRows] = useState<WSTTableRow[]>([]);
  const [policyLoading, setPolicyLoading] = useState(false);
  const [policyError, setPolicyError] = useState<string | null>(null);
  const [programmableBalances, setProgrammableBalances] = useState<PolicyTokenBalance[]>([]);
  const [programmableBalanceLoading, setProgrammableBalanceLoading] = useState(false);
  const [programmableBalanceError, setProgrammableBalanceError] = useState<string | null>(null);
  const [programmableBalanceRefreshKey, setProgrammableBalanceRefreshKey] = useState(0);
  const [selectedTokenHex, setSelectedTokenHex] = useState('');

  const demoEnv = useContext(DemoEnvironmentContext);

  useEffect(() => {
    useStore.getState();
    // console.log("accounts changed:", accounts);
  }, [accounts]);

  useEffect(() => {
    if (!lucid || !accounts.walletUser.regular_address || accounts.walletUser.hasRegisteredScripts === true) {
      return;
    }
    let cancelled = false;
    const verifyRegistration = async () => {
      try {
        const registered = await areStakeScriptsRegistered(demoEnv, lucid, accounts.walletUser.regular_address);
        if (registered && !cancelled) {
          changeWalletAccountDetails('walletUser', {
            ...accounts.walletUser,
            hasRegisteredScripts: true,
          });
        }
      } catch (error) {
        console.warn('Failed to verify stake script registration status', error);
      }
    };
    verifyRegistration();
    return () => {
      cancelled = true;
    };
  }, [accounts.walletUser.hasRegisteredScripts, accounts.walletUser.regular_address, changeWalletAccountDetails, demoEnv, lucid]);

  const getUserAccountDetails = () => {
    switch (currentUser) {
      case "Alice": return accounts.alice;
      case "Bob": return accounts.bob;
      case "Connected Wallet": return accounts.walletUser;
      default: return undefined;
    };
  };

  const getUserMnemonic = () => {
    switch (currentUser) {
      case "Alice": return demoEnv.user_a;
      case "Bob": return demoEnv.user_b;
      case "Mint Authority": return demoEnv.mint_authority;
      case "Connected Wallet": return ""; //TODO: this seems to be broken
      case "Not Connected": return "";
    };

  }

  // temp state for each text field
  const [sendTokenAmount, setMintTokens] = useState(0);
  const [sendRecipientAddress, setsendRecipientAddress] = useState('address');
  const accountDetails = getUserAccountDetails();
  const accountAdaBalance = accountDetails?.balance?.ada ?? 0;
  const accountCollateralCount = accountDetails?.balance?.adaOnlyOutputs ?? 0;
  const connectedWalletPreview = accounts.walletUser.regular_address ? accounts.walletUser.regular_address.slice(0, 15) : '';
  const tokenOptions = useMemo(
    () =>
      programmableBalances.flatMap((group) =>
        group.tokens.map((token) => {
          const quantityBigInt = (() => {
            try {
              return BigInt(token.quantity ?? '0');
            } catch {
              return 0n;
            }
          })();
          return {
            value: token.assetNameHex,
            label: token.displayName,
            policyId: group.policyId,
            quantity: quantityBigInt,
            quantityDisplay: token.quantity,
          };
        })
      ),
    [programmableBalances]
  );

  const selectedTokenOption = tokenOptions.find((option) => option.value === selectedTokenHex);
  const selectedTokenBalance = selectedTokenOption?.quantity ?? 0n;

  const parsedSendAmount = Number.isFinite(sendTokenAmount) ? sendTokenAmount : 0;
  const sendAmountBigInt = (() => {
    if (parsedSendAmount <= 0) {
      return 0n;
    }
    try {
      return BigInt(Math.floor(parsedSendAmount));
    } catch {
      return 0n;
    }
  })();
  const sendAmountValid =
    Boolean(selectedTokenHex) &&
    parsedSendAmount > 0 &&
    sendAmountBigInt > 0n &&
    sendAmountBigInt <= selectedTokenBalance;
  const showInsufficientBalance =
    Boolean(selectedTokenHex) && parsedSendAmount > 0 && sendAmountBigInt > selectedTokenBalance;

  useEffect(() => {
    if (tokenOptions.length === 0) {
      setSelectedTokenHex('');
      return;
    }
    setSelectedTokenHex((prev) =>
      prev && tokenOptions.some((option) => option.value === prev) ? prev : tokenOptions[0].value
    );
  }, [tokenOptions]);

  useEffect(() => {
    if (
      currentUser !== 'Connected Wallet' ||
      !accounts.walletUser.hasRegisteredScripts ||
      !accounts.walletUser.regular_address
    ) {
      setPolicyOptions([]);
      setSelectedPolicy('');
      setPolicyRows([]);
      setPolicyError(null);
      return;
    }
    let cancelled = false;
    const loadPolicyIds = async () => {
      try {
        const freezePolicyId = await getFreezePolicyId(accounts.walletUser.regular_address);
        if (!cancelled) {
          const options = [{ id: freezePolicyId, label: 'Freeze / Seize policy' }];
          setPolicyOptions(options);
          setSelectedPolicy(freezePolicyId);
          setPolicyError(null);
        }
      } catch (error) {
        console.warn('Failed to load freeze policy id', error);
        if (!cancelled) {
          setPolicyError('Unable to load policy identifiers.');
        }
      }
    };
    loadPolicyIds();
    return () => {
      cancelled = true;
    };
  }, [accounts.walletUser.hasRegisteredScripts, accounts.walletUser.regular_address, currentUser]);

  useEffect(() => {
    if (
      currentUser !== 'Connected Wallet' ||
      !accounts.walletUser.hasRegisteredScripts ||
      !selectedPolicy ||
      !lucid
    ) {
      setPolicyRows([]);
      setPolicyLoading(false);
      return;
    }
    let cancelled = false;
    const loadPolicyHoldersData = async () => {
      setPolicyLoading(true);
      setPolicyError(null);
      try {
        const holders = await fetchPolicyHolders(demoEnv, selectedPolicy);
        const rows = await Promise.all(
          holders.map(async (holder) => {
            let programmableAddress = '';
            return {
              regularAddress: '',
              programmableAddress: holder.address,
              status: 'Unknown',
              assets: holder.assets,
            } as WSTTableRow;
          })
        );
        if (!cancelled) {
          setPolicyRows(rows);
        }
      } catch (error) {
        console.warn('Failed to load policy holders', error);
        if (!cancelled) {
          setPolicyError('Unable to load address holdings.');
          setPolicyRows([]);
        }
      } finally {
        if (!cancelled) {
          setPolicyLoading(false);
        }
      }
    };
    loadPolicyHoldersData();
    return () => {
      cancelled = true;
    };
  }, [accounts.walletUser.hasRegisteredScripts, demoEnv, lucid, selectedPolicy, currentUser]);

  useEffect(() => {
    if (currentUser !== 'Connected Wallet' || !accounts.walletUser.regular_address) {
      setProgrammableBalances([]);
      setProgrammableBalanceLoading(false);
      setProgrammableBalanceError(null);
      return;
    }
    let cancelled = false;
    const loadProgrammableBalances = async () => {
      setProgrammableBalanceLoading(true);
      setProgrammableBalanceError(null);
      try {
        const balances = await getUserTotalProgrammableValue(accounts.walletUser.regular_address);
        if (!cancelled) {
          setProgrammableBalances(balances);
        }
      } catch (error) {
        console.warn('Failed to load programmable token balances', error);
        if (!cancelled) {
          setProgrammableBalanceError('Unable to load programmable token balances.');
          setProgrammableBalances([]);
        }
      } finally {
        if (!cancelled) {
          setProgrammableBalanceLoading(false);
        }
      }
    };
    loadProgrammableBalances();
    return () => {
      cancelled = true;
    };
  }, [accounts.walletUser.regular_address, currentUser, programmableBalanceRefreshKey]);

  const onSend = async () => {
    if (getUserAccountDetails()?.status === 'Frozen' && !overrideTx) {
      changeAlertInfo({
        severity: 'error',
        message: 'Cannot send WST with frozen address.',
        open: true,
        link: ''
      });
      return;
    }
    console.log('start sending tokens');
    changeAlertInfo({severity: 'info', message: 'Transaction processing', open: true, link: ''});
    const accountInfo = getUserAccountDetails();
    if (!accountInfo) {
      console.error("No valid send address found! Cannot send.");
      return;
    }
    if (!selectedTokenHex) {
      changeAlertInfo({
        severity: 'error',
        message: 'Select a programmable token to send.',
        open: true,
        link: ''
      });
      return;
    }
    if (sendAmountBigInt === 0n) {
      changeAlertInfo({
        severity: 'error',
        message: 'Enter a token amount greater than zero.',
        open: true,
        link: ''
      });
      return;
    }
    if (sendAmountBigInt > selectedTokenBalance) {
      changeAlertInfo({
        severity: 'error',
        message: 'Insufficient balance for this token.',
        open: true,
        link: ''
      });
      return;
    }
    if (currentUser !== 'Connected Wallet') {
      lucid.selectWallet.fromSeed(getUserMnemonic());
    }
    lucid.wallet().address().then(console.log);
    const tokenPolicyId = selectedTokenOption?.policyId;
    if (!tokenPolicyId) {
      changeAlertInfo({
        severity: 'error',
        message: 'Select a programmable token with a known policy before sending.',
        open: true,
        link: ''
      });
      return;
    }
    let issuerForPolicy: string;
    try {
      issuerForPolicy = await getPolicyIssuer(tokenPolicyId);
    } catch (error) {
      console.error('Failed to resolve policy issuer', error);
      changeAlertInfo({
        severity: 'error',
        message: 'Unable to resolve the issuer for the selected token policy.',
        open: true,
        link: ''
      });
      return;
    }
    const requestData = {
      asset_name: selectedTokenHex,
      issuer: issuerForPolicy,
      quantity: sendTokenAmount,
      recipient: sendRecipientAddress,
      sender: accountInfo.regular_address,
      submit_failing_tx: overrideTx
    };
    try {
      const response = await axios.post(
        '/api/v1/tx/programmable-token/transfer', 
        requestData, 
        {
          headers: {
            'Content-Type': 'application/json;charset=utf-8', 
          },
        }
      );
      console.log('Send response:', response.data);
      const tx = await lucid.fromTx(response.data.cborHex);
      const txId = await signAndSentTx(lucid, tx);
      await updateAccountBalance(sendRecipientAddress);
      await updateAccountBalance(accountInfo.regular_address);
      setProgrammableBalanceRefreshKey((key) => key + 1);
      changeAlertInfo({severity: 'success', message: 'Transaction sent successfully!', open: true, link: `${demoEnv.explorer_url}/${txId}`});
    } catch (error) {
      console.error('Send failed:', error);
    }
  };

  const onRegisterAsset = async () => {
    if (isRegistering) {
      return;
    }
    if (currentUser !== 'Connected Wallet') {
      changeAlertInfo({
        severity: 'error',
        message: 'Switch to the Connected Wallet profile to register a programmable asset.',
        open: true,
        link: ''
      });
      return;
    }
    const issuerAddress = accounts.walletUser.regular_address;
    if (!issuerAddress) {
      changeAlertInfo({
        severity: 'error',
        message: 'Connect Lace to populate your wallet address before registering.',
        open: true,
        link: ''
      });
      return;
    }
    const announceSuccess = async (txId?: string, msg = 'Programmable asset registered successfully!') => {
      const latestAccounts = useStore.getState().accounts;
      let programmableAddress = latestAccounts.walletUser.programmable_token_address;
      let registrationStatus = latestAccounts.walletUser.hasRegisteredScripts;
      try {
        const [updatedAddress, isRegistered] = await Promise.all([
          getProgrammableTokenAddress(issuerAddress),
          areStakeScriptsRegistered(demoEnv, lucid, issuerAddress),
        ]);
        programmableAddress = updatedAddress ?? programmableAddress;
        registrationStatus = isRegistered;
      } catch (err) {
        console.warn('Failed to refresh programmable token metadata after registration', err);
      }
      changeWalletAccountDetails('walletUser', {
        ...latestAccounts.walletUser,
        programmable_token_address: programmableAddress ?? latestAccounts.walletUser.programmable_token_address,
        hasRegisteredScripts: registrationStatus ?? latestAccounts.walletUser.hasRegisteredScripts,
      });
      changeAlertInfo({
        severity: 'success',
        message: msg,
        open: true,
        link: txId ? `${demoEnv.explorer_url}/${txId}` : ''
      });
    };

    try {
      setIsRegistering(true);
      changeAlertInfo({ severity: 'info', message: 'Preparing registration transaction…', open: true, link: '' });
      const response = await axios.post(
        '/api/v1/tx/programmable-token/register-transfer-scripts',
        { issuer: issuerAddress },
        { headers: { 'Content-Type': 'application/json;charset=utf-8' } }
      );
      const tx = await lucid.fromTx(response.data.cborHex);
      const txId = await signAndSentTx(lucid, tx);
      await announceSuccess(txId);
    } catch (error: any) {
      const errPayload = error?.response?.data ?? '';
      const errAsString = typeof errPayload === 'string' ? errPayload : JSON.stringify(errPayload);
      if (errAsString.includes('StakeKeyRegisteredDELEG')) {
        await announceSuccess(undefined, 'Programmable asset scripts already registered.');
        console.warn('Scripts already registered, treating as success.');
      } else {
        console.error('Register asset failed:', error);
        changeAlertInfo({
          severity: 'error',
          message: 'Registration failed. Please try again after checking the console output.',
          open: true,
          link: ''
        });
      }
    } finally {
      setIsRegistering(false);
    }
  };

  const onBlacklistInit = async () => {
    if (isInitializingBlacklist) {
      return;
    }
    if (currentUser !== 'Connected Wallet') {
      changeAlertInfo({
        severity: 'error',
        message: 'Switch to the Connected Wallet profile to initialise the blacklist.',
        open: true,
        link: ''
      });
      return;
    }
    const issuerAddress = accounts.walletUser.regular_address;
    if (!issuerAddress) {
      changeAlertInfo({
        severity: 'error',
        message: 'Connect Lace to populate your wallet address before initialising the blacklist.',
        open: true,
        link: ''
      });
      return;
    }
    try {
      setIsInitializingBlacklist(true);
      changeAlertInfo({ severity: 'info', message: 'Preparing blacklist initialisation transaction…', open: true, link: '' });
      const response = await axios.post(
        '/api/v1/tx/programmable-token/blacklist-init',
        { issuer: issuerAddress },
        { headers: { 'Content-Type': 'application/json;charset=utf-8' } }
      );
      const tx = await lucid.fromTx(response.data.cborHex);
      const txId = await signAndSentTx(lucid, tx);
      changeAlertInfo({
        severity: 'success',
        message: 'Blacklist initialised successfully!',
        open: true,
        link: `${demoEnv.explorer_url}/${txId}`
      });
    } catch (error) {
      console.error('Blacklist init failed:', error);
      changeAlertInfo({
        severity: 'error',
        message: 'Blacklist initialisation failed. Check the console for details.',
        open: true,
        link: ''
      });
    } finally {
      setIsInitializingBlacklist(false);
    }
  };

  const updateAccountBalance = async (address: string) => {
    const newAccountBalance = await getWalletBalance(demoEnv, address);
      const walletKey = (Object.keys(accounts) as (keyof Accounts)[]).find(
        (key) => accounts[key].regular_address === address
      );
      if (walletKey) {
        changeWalletAccountDetails(walletKey, {
          ...accounts[walletKey],
          balance: newAccountBalance,
        });
      }
  };

  const ensureWalletReady = (requireRegistration = true): string | null => {
    const issuerAddress = accounts.walletUser.regular_address;
    if (!issuerAddress) {
      changeAlertInfo({
        severity: 'error',
        message: 'Connect your Lace wallet to continue.',
        open: true,
        link: ''
      });
      return null;
    }
    if (requireRegistration && !accounts.walletUser.hasRegisteredScripts) {
      changeAlertInfo({
        severity: 'error',
        message: 'Register your programmable asset before using mint actions.',
        open: true,
        link: ''
      });
      return null;
    }
    return issuerAddress;
  };

  const handleUserAddressClearedChange = (event: { target: { checked: boolean | ((prevState: boolean) => boolean); }; }) => {
    setUserAddressCleared(event.target.checked);
  };

  const onUserMint = async () => {
    const issuerAddress = ensureWalletReady();
    if (!issuerAddress) return;
    if (!userAddressCleared) {
      changeAlertInfo({
        severity: 'error',
        message: 'Confirm the recipient address is cleared before minting.',
        open: true,
        link: ''
      });
      return;
    }
    changeAlertInfo({severity: 'info', message: 'Processing mint request…', open: true, link: ''});
    const assetLabel = userAssetName.trim() === '' ? 'TOKEN' : userAssetName;
    const recipient = userMintRecipient.trim() !== '' ? userMintRecipient : accounts.walletUser.programmable_token_address || issuerAddress;
    const requestData = {
      asset_name: Buffer.from(assetLabel, 'utf8').toString('hex'),
      issuer: issuerAddress,
      quantity: userMintAmount,
      recipient
    };
    try {
      const response = await axios.post(
        '/api/v1/tx/programmable-token/issue',
        requestData,
        {
          headers: {'Content-Type': 'application/json;charset=utf-8'}
        }
      );
      const tx = await lucid.fromTx(response.data.cborHex);
      const txId = await signAndSentTx(lucid, tx);
      changeAlertInfo({
        severity: 'success',
        message: 'Mint successful!',
        open: true,
        link: `${demoEnv.explorer_url}/${txId}`
      });
      await updateAccountBalance(recipient);
      await updateAccountBalance(issuerAddress);
      setProgrammableBalanceRefreshKey((key) => key + 1);
    } catch (error) {
      console.error('Connected wallet mint failed:', error);
      changeAlertInfo({
        severity: 'error',
        message: 'Mint failed. See console for details.',
        open: true,
        link: ''
      });
    }
  };

  const onUserFreeze = async () => {
    const issuerAddress = ensureWalletReady();
    if (!issuerAddress || !userFreezeAddress) return;
    changeAlertInfo({severity: 'info', message: 'Processing freeze request…', open: true, link: ''});
    const requestData = {
      issuer: issuerAddress,
      blacklist_address: userFreezeAddress,
      reason: userFreezeReason
    };
    try {
      const response = await axios.post(
        '/api/v1/tx/programmable-token/blacklist',
        requestData,
        { headers: {'Content-Type': 'application/json;charset=utf-8'} }
      );
      const tx = await lucid.fromTx(response.data.cborHex);
      const txId = await signAndSentTx(lucid, tx);
      changeAlertInfo({
        severity: 'success',
        message: 'Address frozen.',
        open: true,
        link: `${demoEnv.explorer_url}/${txId}`
      });
      const frozenWalletKey = (Object.keys(accounts) as (keyof Accounts)[]).find(
        (key) => accounts[key].regular_address === userFreezeAddress
      );
      if (frozenWalletKey) {
        changeWalletAccountDetails(frozenWalletKey, {
          ...accounts[frozenWalletKey],
          status: 'Frozen',
        });
      }
    } catch (error: any) {
      if (error?.response?.data?.includes?.('DuplicateBlacklistNode')) {
        changeAlertInfo({
          severity: 'error',
          message: 'This address is already frozen.',
          open: true,
          link: ''
        });
      } else {
        console.error('Connected wallet freeze failed:', error);
      }
    }
  };

  const onUserUnfreeze = async () => {
    const issuerAddress = ensureWalletReady();
    if (!issuerAddress || !userUnfreezeAddress) return;
    changeAlertInfo({severity: 'info', message: 'Processing unfreeze request…', open: true, link: ''});
    const requestData = {
      issuer: issuerAddress,
      blacklist_address: userUnfreezeAddress,
      reason: '(unfreeze)'
    };
    try {
      const response = await axios.post(
        '/api/v1/tx/programmable-token/unblacklist',
        requestData,
        { headers: {'Content-Type': 'application/json;charset=utf-8'} }
      );
      const tx = await lucid.fromTx(response.data.cborHex);
      const txId = await signAndSentTx(lucid, tx);
      changeAlertInfo({
        severity: 'success',
        message: 'Address unfrozen.',
        open: true,
        link: `${demoEnv.explorer_url}/${txId}`
      });
      const unfrozenWalletKey = (Object.keys(accounts) as (keyof Accounts)[]).find(
        (key) => accounts[key].regular_address === userUnfreezeAddress
      );
      if (unfrozenWalletKey) {
        changeWalletAccountDetails(unfrozenWalletKey, {
          ...accounts[unfrozenWalletKey],
          status: 'Active',
        });
      }
    } catch (error: any) {
      if (error?.response?.data?.includes?.('BlacklistNodeNotFound')) {
        changeAlertInfo({
          severity: 'error',
          message: 'This address is not frozen.',
          open: true,
          link: ''
        });
      } else {
        console.error('Connected wallet unfreeze failed:', error);
      }
    }
  };

  const onUserSeize = async () => {
    const issuerAddress = ensureWalletReady();
    if (!issuerAddress || !userSeizeAddress) return;
    changeAlertInfo({severity: 'info', message: 'Processing seizure request…', open: true, link: ''});
    const requestData = {
      issuer: issuerAddress,
      target: userSeizeAddress,
      reason: userSeizeReason
    };
    try {
      const response = await axios.post(
        '/api/v1/tx/programmable-token/seize',
        requestData,
        { headers: {'Content-Type': 'application/json;charset=utf-8'} }
      );
      const tx = await lucid.fromTx(response.data.cborHex);
      const txId = await signAndSentTx(lucid, tx);
      await updateAccountBalance(userSeizeAddress);
      changeAlertInfo({
        severity: 'success',
        message: 'Funds seized.',
        open: true,
        link: `${demoEnv.explorer_url}/${txId}`
      });
    } catch (error) {
      console.error('Connected wallet seize failed:', error);
      changeAlertInfo({
        severity: 'error',
        message: 'Seizure failed. See console for details.',
        open: true,
        link: ''
      });
    }
  };
  
  const sendContent =  <Box>
  <FormControl fullWidth sx={{ mb: 1 }} size="small">
    <InputLabel id="token-select-label">Token to Send</InputLabel>
    <Select
      labelId="token-select-label"
      value={selectedTokenHex}
      label="Token to Send"
      onChange={(event: SelectChangeEvent) => setSelectedTokenHex(event.target.value)}
      disabled={tokenOptions.length === 0}
    >
      {tokenOptions.map((option) => (
        <MenuItem key={`${option.policyId}-${option.value}`} value={option.value}>
          {option.label}
        </MenuItem>
      ))}
    </Select>
  </FormControl>
  {selectedTokenOption && (
    <Typography variant='caption' color='text.secondary' sx={{ display: 'block', mb: 2 }}>
      Balance: {selectedTokenOption.quantityDisplay}
    </Typography>
  )}
  <WSTTextField 
      placeholder='0.0000'
      value={sendTokenAmount}
      onChange={(e) => setMintTokens(Number(e.target.value))}
      label="Number of Tokens to Send"
      fullWidth={true}
      error={showInsufficientBalance}
      helperText={showInsufficientBalance ? 'Insufficient balance' : undefined}
  />
  <WSTTextField 
      placeholder="address"
      value={sendRecipientAddress}
      onChange={(e) => setsendRecipientAddress(e.target.value)}
      label="Recipient’s Address"
      fullWidth={true}
  />
    <FormControlLabel
      control={<Checkbox size="small" checked={overrideTx} onChange={x => setOverrideTx(x.target.checked)} />}
      label="⚠️ Force send failing transaction"
      sx={{ mb: 2 }}
  />
  
  </Box>;

  const receiveContent =  <Box>
    <CopyTextField 
      value={accountDetails?.regular_address ?? ''}
      fullWidth={true}
      label="Regular Address"
      />
  </Box>;

  const userMintContent = (
    <Box>
      <WSTTextField
        value={userAssetName}
        onChange={(e) => setUserAssetName(e.target.value)}
        label="Asset Symbol"
        fullWidth={true}
      />
      <WSTTextField
        value={userMintAmount}
        onChange={(e) => setUserMintAmount(Number(e.target.value))}
        label="Number of Tokens to Mint"
        fullWidth={true}
      />
      <WSTTextField
        value={userMintRecipient}
        onChange={(e) => setUserMintRecipient(e.target.value)}
        label="Mint Recipient’s Address (optional)"
        fullWidth={true}
      />
      <FormControlLabel
        control={<Checkbox size="small" checked={userAddressCleared} onChange={handleUserAddressClearedChange} />}
        label="Recipient address has been cleared"
        sx={{ mb: 2 }}
      />
    </Box>
  );

  const userFreezeContent = (
    <Box>
      <WSTTextField
        value={userFreezeAddress}
        onChange={(e) => setUserFreezeAddress(e.target.value)}
        label="Address to Freeze"
        fullWidth={true}
      />
      <WSTTextField
        value={userFreezeReason}
        onChange={(e) => setUserFreezeReason(e.target.value)}
        label="Reason"
        fullWidth={true}
      />
    </Box>
  );

  const userUnfreezeContent = (
    <Box>
      <WSTTextField
        value={userUnfreezeAddress}
        onChange={(e) => setUserUnfreezeAddress(e.target.value)}
        label="Address to Unfreeze"
        fullWidth={true}
      />
    </Box>
  );

  const userSeizeContent = (
    <Box>
      <WSTTextField
        value={userSeizeAddress}
        onChange={(e) => setUserSeizeAddress(e.target.value)}
        label="Address to Seize"
        fullWidth={true}
      />
      <WSTTextField
        value={userSeizeReason}
        onChange={(e) => setUserSeizeReason(e.target.value)}
        label="Reason"
        fullWidth={true}
      />
    </Box>
  );

  const registerContent = (
    <Box>
      <Typography variant='body1' sx={{ mb: 2 }}>
        Register your programmable asset scripts using the connected Lace wallet. This registers the stake scripts that will allow you to mint your own programmable token. 
      </Typography>
      <CopyTextField
        value={accounts.walletUser.regular_address ?? ''}
        fullWidth={true}
        label="Connected Wallet Address"
      />
    </Box>
  );

  const blacklistInitContent = (
    <Box>
      <Typography variant='body1' sx={{ mb: 2 }}>
        Initialise the blacklist state for your programmable asset to enable future freeze and seize actions.
      </Typography>
      <CopyTextField
        value={accounts.walletUser.regular_address ?? ''}
        fullWidth={true}
        label="Connected Wallet Address"
      />
    </Box>
  );

  const renderProgrammableBalanceCards = () => {
    if (programmableBalanceLoading) {
      return (
        <Box sx={{ display: 'flex', justifyContent: 'center', alignItems: 'center', minHeight: 120 }}>
          <CircularProgress size={24} />
        </Box>
      );
    }
    if (programmableBalanceError) {
      return (
        <Typography variant='body2' color='error'>
          {programmableBalanceError}
        </Typography>
      );
    }
    if (programmableBalances.length === 0) {
      return (
        <Typography variant='body2' color='text.secondary'>
          No programmable tokens detected for this wallet yet.
        </Typography>
      );
    }
    return (
      <Box sx={{ display: 'flex', flexWrap: 'wrap', gap: 2 }}>
        {programmableBalances.map((group) => (
          <Paper key={group.policyId} sx={{ flex: '1 1 320px', p: 2, borderRadius: 2, border: '1px solid #E0E0E0' }}>
            <Typography variant='subtitle2' color='text.secondary'>
              Policy ID
            </Typography>
            <Typography variant='body2' sx={{ fontFamily: 'monospace', wordBreak: 'break-all', mb: 1 }}>
              {group.policyId}
            </Typography>
            {group.tokens.map((token, index) => (
              <Box
                key={`${group.policyId}-${token.assetNameHex}-${index}`}
                sx={{
                  display: 'flex',
                  justifyContent: 'space-between',
                  alignItems: 'center',
                  py: 1,
                  borderTop: index === 0 ? '1px solid #EFEFEF' : 'none',
                }}
              >
                <Box sx={{ pr: 2 }}>
                  <Typography variant='body1' fontWeight={600}>
                    {token.displayName}
                  </Typography>
                  <Typography variant='caption' color='text.secondary' sx={{ fontFamily: 'monospace' }}>
                    {token.assetNameHex || '—'}
                  </Typography>
                </Box>
                <Typography variant='body1' sx={{ fontFamily: 'monospace' }}>
                  {token.quantity}
                </Typography>
              </Box>
            ))}
          </Paper>
        ))}
      </Box>
    );
  };

  const renderWalletView = () => (
    <>
      <Box sx={{display: 'flex', justifyContent: 'space-between', alignItems: 'end', marginBottom: '16px'}}>
        <Box>
          <Typography variant='h4'>Address Balance</Typography>
          <Typography variant='h1'>{accountAdaBalance} Ada { (accountCollateralCount === 0) && (<span>({accountCollateralCount} collateral UTxOs)</span>)}</Typography>
        </Box>
        <Typography variant='h5'>{accountDetails?.regular_address ? accountDetails.regular_address.slice(0,15) : ''}</Typography>
      </Box>
      <div className="cardWrapperParent">
        <WalletCard tabs={[
          {
            label: 'Send',
            content: sendContent,
            buttonLabel: 'Send',
            onAction: onSend,
            buttonDisabled: !sendAmountValid
          },
          {
            label: 'Receive',
            content: receiveContent
          }
        ]}/>
      </div>
      {currentUser === 'Connected Wallet' && (
        <Box sx={{ mt: 4 }}>
          <Typography variant='h5'>Programmable Token Balances</Typography>
          <Typography variant='body2' color='text.secondary' sx={{ mb: 2 }}>
            Totals for every programmable token held by your connected wallet, grouped by policy ID.
          </Typography>
          {renderProgrammableBalanceCards()}
        </Box>
      )}
    </>
  );

  const renderMintActionsView = () => {
    if (currentUser !== 'Connected Wallet') {
      return (
        <Box sx={{marginBottom: '16px'}}>
          <Typography variant='body1'>Mint actions are only available when using the Connected Wallet profile.</Typography>
        </Box>
      );
    }

    if (!accounts.walletUser.hasRegisteredScripts) {
      return (
        <Box sx={{marginBottom: '16px'}}>
          <Typography variant='body1'>
            Register your freeze and seize policy before accessing mint actions.
          </Typography>
        </Box>
      );
    }

    return (
      <>
        <Box sx={{display: 'flex', justifyContent: 'space-between', alignItems: 'end', marginBottom: '16px'}}>
          <Box>
            <Typography variant='h4'>Freeze and Seize Token Controls</Typography>
            <Typography variant='body1'>Manage tokens issued by your connected Lace wallet.</Typography>
          </Box>
          <Typography variant='h5'>{connectedWalletPreview}</Typography>
        </Box>
        <div className="cardWrapperParent">
          <WalletCard tabs={[
            {
              label: 'Mint',
              content: userMintContent,
              buttonLabel: 'Mint',
              onAction: onUserMint
            },
            {
              label: 'Freeze',
              content: userFreezeContent,
              buttonLabel: 'Freeze',
              onAction: onUserFreeze
            },
            {
              label: 'Unfreeze',
              content: userUnfreezeContent,
              buttonLabel: 'Unfreeze',
              onAction: onUserUnfreeze
            },
            {
              label: 'Seize',
              content: userSeizeContent,
              buttonLabel: 'Seize',
              onAction: onUserSeize
            }
          ]}/>
        </div>
      </>
    );
  };

  const renderAddressesView = () => {
    if (currentUser !== 'Connected Wallet') {
      return (
        <Box sx={{marginBottom: '16px'}}>
          <Typography variant='body1'>Switch to the Connected Wallet profile to view policy holdings.</Typography>
        </Box>
      );
    }
    if (!accounts.walletUser.hasRegisteredScripts) {
      return (
        <Box sx={{marginBottom: '16px'}}>
          <Typography variant='body1'>
            Register your programmable asset to view address holdings.
          </Typography>
        </Box>
      );
    }
    return (
      <>
        <Box sx={{display: 'flex', justifyContent: 'space-between', alignItems: 'center', marginBottom: '16px', gap: 2, flexWrap: 'wrap'}}>
          <Box>
            <Typography variant='h4'>Addresses Holding Your Programmable Token</Typography>
            <Typography variant='body1'>
              Select a policy to inspect all holders and their balances.
            </Typography>
          </Box>
          <FormControl size="small" sx={{ minWidth: 240 }}>
            <InputLabel id="policy-select-label">Policy</InputLabel>
            <Select
              labelId="policy-select-label"
              value={selectedPolicy}
              label="Policy"
              onChange={(event) => setSelectedPolicy(event.target.value)}
              disabled={policyOptions.length === 0}
            >
              {policyOptions.map((option) => (
                <MenuItem key={option.id} value={option.id}>
                  {option.label}
                </MenuItem>
              ))}
            </Select>
          </FormControl>
        </Box>
        {selectedPolicy && (
          <Box sx={{ mb: 2 }}>
            <CopyTextField
              value={selectedPolicy}
              label="Token Policy ID"
              fullWidth={true}
            />
          </Box>
        )}
        {policyError && (
          <Typography variant='body2' color='error' sx={{ mb: 2 }}>
            {policyError}
          </Typography>
        )}
        <WSTTable rows={policyRows} loading={policyLoading} emptyMessage="No holders found for this policy." />
      </>
    );
  };

  const renderRegisterAssetView = () => (
    <>
      <Box sx={{marginBottom: '16px'}}>
        <Typography variant='h4'>Register Freeze and Seize Programmable Asset</Typography>
        <Typography variant='body1'>Use your connected Lace wallet to register the programmable token scripts.</Typography>
      </Box>
      <div className="cardWrapperParent">
        <WalletCard tabs={[
          {
            label: 'Register',
            content: registerContent,
            buttonLabel: isRegistering ? 'Registering…' : 'Register Asset',
            onAction: onRegisterAsset,
            buttonDisabled: isRegistering
          },
          {
            label: 'Blacklist Init',
            content: blacklistInitContent,
            buttonLabel: isInitializingBlacklist ? 'Initializing…' : 'Initialize Blacklist',
            onAction: onBlacklistInit,
            buttonDisabled: isInitializingBlacklist
          }
        ]}/>
      </div>
    </>
  );

  const renderInactiveState = () => (
    <Box sx={{ p: 4 }}>
      <Typography variant='h4' sx={{ mb: 1 }}>Connect a Wallet to Continue</Typography>
      <Typography variant='body1'>
        Use the Connect Wallet button in the top-right corner to link Lace, Eternl, or Yoroi before accessing wallet features.
      </Typography>
    </Box>
  );

  return (
    <div className="page">
      {!selectedTab
        ? renderInactiveState()
        : selectedTab === 'Register Asset'
          ? renderRegisterAssetView()
          : selectedTab === 'Mint Actions'
            ? renderMintActionsView()
            : selectedTab === 'Addresses'
              ? renderAddressesView()
              : renderWalletView()}
    </div>
  );
}
