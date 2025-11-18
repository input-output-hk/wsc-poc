'use client';
//React imports
import React, { useCallback, useContext, useEffect, useMemo, useState } from 'react';

//Axios imports
import axios from 'axios';

//Lucid imports
import { paymentCredentialOf } from '@lucid-evolution/lucid';
import type { Credential as LucidCredential } from "@lucid-evolution/core-types";

//Mui imports
import { Box, Typography } from '@mui/material';
import Checkbox from '@mui/material/Checkbox';
import FormControlLabel from '@mui/material/FormControlLabel';

//Local components
import useStore from '../store/store'; 
import { Accounts } from '../store/types';
import WalletCard from '../components/Card';
import WSTTextField from '../components/WSTTextField';
import CopyTextField from '../components/CopyTextField';
import WSTTable, { WSTTableRow } from '../components/WSTTable';
import { getWalletBalance, signAndSentTx, getBlacklist } from '../utils/walletUtils';
import DemoEnvironmentContext from '../context/demoEnvironmentContext';



export default function Home() {
  const lucid = useStore((state) => state.lucid);
  const mintAccount = useStore((state) => state.mintAccount);
  const accounts = useStore((state) => state.accounts);
  const selectedTab = useStore((state) => state.selectedTab);
  const changeAlertInfo = useStore((state) => state.changeAlertInfo);
  const changeMintAccountDetails = useStore((state) => state.changeMintAccountDetails);
  const changeWalletAccountDetails = useStore((state) => state.changeWalletAccountDetails);
  const changeUserAccount = useStore((state) => state.changeUserAccount);
  const addressRows = useMemo<WSTTableRow[]>(() => {
    return Object.values(accounts)
      .filter((acct) => acct.regular_address !== '')
      .map((acct) => ({
        regularAddress: acct.regular_address,
        programmableAddress: acct.programmable_token_address,
        status: acct.status ?? 'Active',
        balanceText: `${acct.balance.wst} WST`,
      }));
  }, [accounts]);
  const [addressCleared, setAddressCleared] = useState(false);
  // Temporary state for each text field
  const [mintTokensAmount, setMintTokens] = useState(0);
  const [sendTokensAmount, setSendTokens] = useState(0);
  const [mintRecipientAddress, setMintRecipientAddress] = useState('mint recipient address');
  const [sendRecipientAddress, setsendRecipientAddress] = useState('send recipient address');
  const [freezeAccountNumber, setFreezeAccountNumber] = useState('address to freeze');
  const [unfreezeAccountNumber, setUnfreezeAccountNumber] = useState('address to unfreeze');
  const [freezeReason, setFreezeReason] = useState('Enter reason here');
  const [seizeAccountNumber, setSeizeAccountNumber] = useState('address to seize');
  const [seizeReason, setSeizeReason] = useState('Enter reason here');
  const [authorityMintPending, setAuthorityMintPending] = useState(false);
  const [authoritySendPending, setAuthoritySendPending] = useState(false);
  const [authorityFreezePending, setAuthorityFreezePending] = useState(false);
  const [authorityUnfreezePending, setAuthorityUnfreezePending] = useState(false);
  const [authoritySeizePending, setAuthoritySeizePending] = useState(false);

  const demoEnv = useContext(DemoEnvironmentContext);
  
  useEffect(() => {
    if (!accounts.walletUser.regular_address) {
      return;
    }
    changeUserAccount('Mint Authority');
  }, [accounts.walletUser.regular_address, changeUserAccount]);

  const fetchUserDetails = useCallback(async () => {
    const mintBalance = await getWalletBalance(demoEnv, mintAccount.regular_address);
    const userABalance = await getWalletBalance(demoEnv, accounts.alice.regular_address);
    const userBBalance = await getWalletBalance(demoEnv, accounts.bob.regular_address);

    // Update Zustand store with the initialized wallet information
    await changeMintAccountDetails({ ...mintAccount, balance: mintBalance});
    await changeWalletAccountDetails('alice', { ...accounts.alice, balance: userABalance});
    await changeWalletAccountDetails('bob', { ...accounts.bob, balance: userBBalance});
  }, [accounts.alice, accounts.bob, changeMintAccountDetails, changeWalletAccountDetails, demoEnv, mintAccount]);

  useEffect(() => {
    const initialize = async () => {
      await fetchUserDetails();
      await fetchBlacklistStatus();
    };
    initialize();
  }, [demoEnv, fetchUserDetails]);

  const fetchBlacklistStatus = async () => {
    const { accounts, changeWalletAccountDetails, currentUser, mintAccount } = useStore.getState();
    let targetAddress: string | undefined;
    if (currentUser === 'Connected Wallet') {
      targetAddress = accounts.walletUser.regular_address;
    } else if (currentUser === 'Mint Authority') {
      targetAddress = mintAccount.regular_address;
    }

    if (!targetAddress || targetAddress.trim() === '') {
      console.warn('No wallet address available for blacklist lookup.');
      return;
    }

    let blacklist = await getBlacklist(targetAddress);
    // if blacklist is not a list then make it an empty list 
    if (!Array.isArray(blacklist)) {
      console.log("Blacklist response incorrect: ", blacklist);
      blacklist = [];
    }
    
    Object.entries(accounts).map(async ([key, account]) => {
      if (!account.regular_address || account.regular_address.trim() === "") {
        // console.log(`${key} has no address yet, skipping`);
        return;
      }
      const credential : LucidCredential = await paymentCredentialOf(account.regular_address);
      if(blacklist && blacklist.includes(credential.hash)) {
        // console.log('a match was found', key as keyof typeof accounts);
        changeWalletAccountDetails(key as keyof typeof accounts, { ...account, status: 'Frozen',});
      }
    });
  };

  const handleAddressClearedChange = (event: { target: { checked: boolean | ((prevState: boolean) => boolean); }; }) => {
    setAddressCleared(event.target.checked);
  };

  const onMint = async () => {
    if (authorityMintPending) {
      return;
    }
    if (addressCleared === false) {
      // setAlertStatus(true);
      console.error("Recipient Address not cleared.");
      return;
    }
    changeAlertInfo({severity: 'info', message: 'Processing Mint Request', open: true, link: ''});
    lucid.selectWallet.fromSeed(demoEnv.mint_authority);
    const requestData = {
      asset_name: Buffer.from('WST', 'utf8').toString('hex'), // Convert "WST" to hex
      issuer: mintAccount.regular_address,
      quantity: mintTokensAmount,
      recipient: mintRecipientAddress
    };

    setAuthorityMintPending(true);
    try {
      const response = await axios.post(
        '/api/v1/tx/programmable-token/issue', 
        requestData, 
        {
          headers: {
            'Content-Type': 'application/json;charset=utf-8', 
          },
        }
      );
      console.log('Mint response:', response.data);
      const tx = await lucid.fromTx(response.data.cborHex);
      const txId = await signAndSentTx(lucid, tx);
      
      changeAlertInfo({
        severity: 'success',
        message: 'Successful new WST mint.',
        open: true,
        link: `${demoEnv.explorer_url}/${txId}`,
        actionText: 'View on Explorer'
      });
      
      await fetchUserDetails();
    } catch (error) {
      console.error('Minting failed:', error);
    } finally {
      setAuthorityMintPending(false);
    }
  };

  const onSend = async () => {
    if (authoritySendPending) {
      return;
    }
    lucid.selectWallet.fromSeed(demoEnv.mint_authority);
    console.log('send tokens');
    changeAlertInfo({severity: 'info', message: 'Transaction processing', open: true, link: ''});
    const requestData = {
      asset_name: Buffer.from('WST', 'utf8').toString('hex'), // Convert "WST" to hex
      issuer: mintAccount.regular_address,
      quantity: sendTokensAmount,
      recipient: sendRecipientAddress,
      sender: mintAccount.regular_address,
    };
    setAuthoritySendPending(true);
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
      const newAccountBalance = await getWalletBalance(demoEnv, sendRecipientAddress);
      const recipientWalletKey = (Object.keys(accounts) as (keyof Accounts)[]).find(
        (key) => accounts[key].regular_address === sendRecipientAddress
      );
      if (recipientWalletKey) {
        changeWalletAccountDetails(recipientWalletKey, {
          ...accounts[recipientWalletKey],
          balance: newAccountBalance,
        });
      }
      changeAlertInfo({
        severity: 'success',
        message: 'Transaction sent successfully!',
        open: true,
        link: `${demoEnv.explorer_url}/${txId}`,
        actionText: 'View on Explorer'
      });
      await fetchUserDetails();
    } catch (error) {
      console.error('Send failed:', error);
    } finally {
      setAuthoritySendPending(false);
    }
  };

  const onFreeze = async () => {
    if (authorityFreezePending) {
      return;
    }
    console.log('freeze an address');
    lucid.selectWallet.fromSeed(demoEnv.mint_authority);
    changeAlertInfo({severity: 'info', message: 'Freeze request processing', open: true, link: ''});
    const requestData = {
      issuer: mintAccount.regular_address,
      blacklist_address: freezeAccountNumber,
      reason: freezeReason,
    };
    setAuthorityFreezePending(true);
    try {
      const response = await axios.post(
        '/api/v1/tx/programmable-token/blacklist', 
        requestData, 
        {
          headers: {
            'Content-Type': 'application/json;charset=utf-8', 
          },
        }
      );
      console.log('Freeze response:', response.data);
      const tx = await lucid.fromTx(response.data.cborHex);
      const txId = await signAndSentTx(lucid, tx);
      console.log(txId);
      changeAlertInfo({
        severity: 'success',
        message: 'Address successfully frozen',
        open: true,
        link: `${demoEnv.explorer_url}/${txId}`,
        actionText: 'View on Explorer'
      });
      const frozenWalletKey = (Object.keys(accounts) as (keyof Accounts)[]).find(
        (key) => accounts[key].regular_address === freezeAccountNumber
      );
      if (frozenWalletKey) {
        changeWalletAccountDetails(frozenWalletKey, {
          ...accounts[frozenWalletKey],
          status: 'Frozen',
        });
      }
    } catch (error: any) {
      if (error.response.data.includes('DuplicateBlacklistNode')) {
        changeAlertInfo({
          severity: 'error',
          message: 'This account is already frozen.',
          open: true,
        });
        return;
      } else {
        console.error('Freeze failed:', error);
      }
    } finally {
      setAuthorityFreezePending(false);
    }
  };

  const onUnfreeze = async () => {
    if (authorityUnfreezePending) {
      return;
    }
    console.log('unfreeze an account');
    lucid.selectWallet.fromSeed(demoEnv.mint_authority);
    changeAlertInfo({severity: 'info', message: 'Unfreeze request processing', open: true, link: ''});
    const requestData = {
      issuer: mintAccount.regular_address,
      blacklist_address: unfreezeAccountNumber,
      reason: "(unfreeze)"
    };
    setAuthorityUnfreezePending(true);
    try {
      const response = await axios.post(
        '/api/v1/tx/programmable-token/unblacklist', 
        requestData, 
        {
          headers: {
            'Content-Type': 'application/json;charset=utf-8', 
          },
        }
      );
      console.log('Unfreeze response:', response.data);
      const tx = await lucid.fromTx(response.data.cborHex);
      const txId = await signAndSentTx(lucid, tx);
      changeAlertInfo({
        severity: 'success',
        message: 'Address successfully unfrozen',
        open: true,
        link: `${demoEnv.explorer_url}/${txId}`,
        actionText: 'View on Explorer'
      });
      const unfrozenWalletKey = (Object.keys(accounts) as (keyof Accounts)[]).find(
        (key) => accounts[key].regular_address === freezeAccountNumber
      );
      if (unfrozenWalletKey) {
        changeWalletAccountDetails(unfrozenWalletKey, {
          ...accounts[unfrozenWalletKey],
          status: 'Active',
        });
      }
    } catch (error: any) {
      if (error.response.data.includes('BlacklistNodeNotFound')) {
        changeAlertInfo({
          severity: 'error',
          message: 'This account is not frozen.',
          open: true,
        });
        return;
      } else {
        console.error('Unfreeze failed:', error);
      }
    } finally {
      setAuthorityUnfreezePending(false);
    }
  };

  const onSeize = async () => {
    if (authoritySeizePending) {
      return;
    }
    console.log('seize account funds');
    lucid.selectWallet.fromSeed(demoEnv.mint_authority);
    changeAlertInfo({severity: 'info', message: 'WST seizure processing', open: true, link: ''});
    const requestData = {
      issuer: mintAccount.regular_address,
      target: seizeAccountNumber,
      reason: seizeReason,
    };
    setAuthoritySeizePending(true);
    try {
      const response = await axios.post(
        '/api/v1/tx/programmable-token/seize', 
        requestData, 
        {
          headers: {
            'Content-Type': 'application/json;charset=utf-8', 
          },
        }
      );
      console.log('Seize response:', response.data);
      const tx = await lucid.fromTx(response.data.cborHex);
      const txId = await signAndSentTx(lucid, tx);
      const newAccountBalance = await getWalletBalance(demoEnv, seizeAccountNumber);
      const seizeWalletKey = (Object.keys(accounts) as (keyof Accounts)[]).find(
        (key) => accounts[key].regular_address === seizeAccountNumber
      );
      if (seizeWalletKey) {
        changeWalletAccountDetails(seizeWalletKey, {
          ...accounts[seizeWalletKey],
          balance: newAccountBalance,
        });
      }
      changeAlertInfo({
        severity: 'success',
        message: 'Funds successfully seized',
        open: true,
        link: `${demoEnv.explorer_url}/${txId}`,
        actionText: 'View on Explorer'
      });
      await fetchUserDetails();
    } catch (error) {
      console.error('Seize failed:', error);
    } finally {
      setAuthoritySeizePending(false);
    }
  };

  const mintContent =  <Box>
  <WSTTextField 
      value={mintTokensAmount}
      onChange={(e) => setMintTokens(Number(e.target.value))}
      label="Number of Tokens to Mint"
      fullWidth={true}
  />
  <WSTTextField 
      value={mintRecipientAddress}
      onChange={(e) => setMintRecipientAddress(e.target.value)}
      label="Mint Recipient’s Address"
      fullWidth={true}
  />
    <FormControlLabel
      control={<Checkbox size="small" checked={addressCleared} onChange={handleAddressClearedChange} />}
      label="Address has been cleared"
      sx={{ mb: 2 }}
  />
  </Box>;

  const freezeContent =  <Box>
  <WSTTextField 
  value={freezeAccountNumber}
  onChange={(e) => setFreezeAccountNumber(e.target.value)}
  label="Address"
  fullWidth={true}
  />
  <WSTTextField 
  value={freezeReason}
  onChange={(e) => setFreezeReason(e.target.value)}
  label="Reason"
  fullWidth={true}
  multiline={true}
  minRows={2}
  maxRows={3}
  />
  </Box>

  const unFreezeContent =  <Box>
  <WSTTextField 
  value={unfreezeAccountNumber}
  onChange={(e) => setUnfreezeAccountNumber(e.target.value)}
  label="Address"
  fullWidth={true}
  />
  </Box>

const seizeContent =  <Box>
<WSTTextField 
value={seizeAccountNumber}
onChange={(e) => setSeizeAccountNumber(e.target.value)}
label="Address"
fullWidth={true}
/>
<WSTTextField 
value={seizeReason}
onChange={(e) => setSeizeReason(e.target.value)}
label="Reason"
fullWidth={true}
multiline={true}
minRows={2}
maxRows={3}
/>
</Box>
  
  const sendContent =  <Box>
  <WSTTextField 
      value={sendTokensAmount}
      onChange={(e) => setSendTokens(Number(e.target.value))}
      label="Number of Tokens to Send"
      fullWidth={true}
  />
  <WSTTextField 
      value={sendRecipientAddress}
      onChange={(e) => setsendRecipientAddress(e.target.value)}
      label="Recipient’s Address"
      fullWidth={true}
  />
  </Box>;

  const receiveContent =  <Box>
    <CopyTextField 
      value={mintAccount.regular_address}
      fullWidth={true}
      label="Your Address"
      />
  </Box>;

  const renderInactiveState = () => (
    <Box sx={{ p: 4 }}>
      <Typography variant='h4' sx={{ mb: 1 }}>Connect a Wallet to Continue</Typography>
      <Typography variant='body1'>
        Use the Connect Wallet button in the top-right corner to link Lace, Eternl, or Yoroi before accessing mint-authority tools.
      </Typography>
    </Box>
  );

  const getContentComponent = () => {
    if (!selectedTab) {
      return renderInactiveState();
    }
    switch (selectedTab) {
      case 'Mint Actions':
        return <>
          <Box sx={{display: 'flex', justifyContent: 'space-between', alignItems: 'end', marginBottom: '16px'}}>
          <Box>
            <Typography variant='h4'>Mint Authority Balance</Typography>
            <Typography variant='h1'>{mintAccount.balance.wst} WST</Typography>
            <Typography variant='h5'>{mintAccount.balance.ada} Ada { (mintAccount.balance.adaOnlyOutputs === 0) && (<span>({mintAccount.balance.adaOnlyOutputs} collateral UTxOs)</span>)}</Typography>
          </Box>
          <Typography variant='h5'>UserID: {mintAccount.regular_address.slice(0,15)}</Typography>
          </Box>
          <div className="cardWrapperParent">
            <WalletCard tabs={[
          {
            label: 'Mint',
            content: mintContent,
            buttonLabel: authorityMintPending ? 'Minting…' : 'Mint',
            onAction: onMint,
            buttonDisabled: authorityMintPending,
            buttonLoading: authorityMintPending
          },
          {
            label: 'Freeze',
            content: freezeContent,
            buttonLabel: authorityFreezePending ? 'Freezing…' : 'Freeze',
            onAction: onFreeze,
            buttonDisabled: authorityFreezePending,
            buttonLoading: authorityFreezePending
          },
          {
            label: 'Unfreeze',
            content: unFreezeContent,
            buttonLabel: authorityUnfreezePending ? 'Unfreezing…' : 'Unfreeze',
            onAction: onUnfreeze,
            buttonDisabled: authorityUnfreezePending,
            buttonLoading: authorityUnfreezePending
          },
          {
            label: 'Seize',
            content: seizeContent,
            buttonLabel: authoritySeizePending ? 'Seizing…' : 'Seize',
            onAction: onSeize,
            buttonDisabled: authoritySeizePending,
            buttonLoading: authoritySeizePending
          }
        ]}></WalletCard>
            <WalletCard tabs={[
              {
                label: 'Send',
                content: sendContent,
                buttonLabel: authoritySendPending ? 'Sending…' : 'Send',
                onAction: onSend,
                buttonDisabled: authoritySendPending,
                buttonLoading: authoritySendPending
              },
              {
                label: 'Receive',
                content: receiveContent
              }
            ]}/>
          </div>
        </>;
      case 'Addresses':
        return <>
          <Box sx={{marginBottom: '16px'}}>
            <Typography variant='h1'>Addresses</Typography>
          </Box> 
          <WSTTable rows={addressRows} />
        </>;
      default:
        return renderInactiveState();
    }
  };

  return (
    <div className="page">
      {getContentComponent()}
    </div>
  );
}


