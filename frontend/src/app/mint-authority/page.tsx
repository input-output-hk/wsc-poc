'use client';
//React imports
import React, { useEffect, useState } from 'react';

//Axios imports
import axios from 'axios';

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
import WSTTable from '../components/WSTTable';
import AlertBar from '../components/AlertBar';
import { getWalletBalance, signAndSentTx, getBlacklist } from '../utils/walletUtils';

export default function Home() {
  const { lucid, mintAccount, accounts, selectedTab, errorMessage, setAlertStatus, changeWalletAccountDetails, fetchBlacklistStatus } = useStore();
  const [addressCleared, setAddressCleared] = useState(false);
  // Temporary state for each text field
  const [mintTokensAmount, setMintTokens] = useState(0);
  const [sendTokensAmount, setSendTokens] = useState(0);
  const [mintRecipientAddress, setMintRecipientAddress] = useState('mint recipient address');
  const [sendRecipientAddress, setsendRecipientAddress] = useState('send recipient address');
  const [freezeAccountNumber, setFreezeAccountNumber] = useState('account to freeze');
  const [freezeReason, setFreezeReason] = useState('Enter reason here');
  const [seizeAccountNumber, setSeizeAccountNumber] = useState('account to seize');
  const [seizeReason, setSeizeReason] = useState('Enter reason here');

  const handleAddressClearedChange = (event: { target: { checked: boolean | ((prevState: boolean) => boolean); }; }) => {
    setAddressCleared(event.target.checked);
  };

  useEffect(() => {
    // getBlacklist();
    useStore.getState();
    // console.log("accounts changed:", accounts, mintAccount);
  }, [accounts, mintAccount]);

  const onMint = async () => {
    if (addressCleared === false) {
      // setAlertStatus(true);
      console.error("Recipient Address not cleared.");
      return;
    }
    lucid.selectWallet.fromSeed(mintAccount.mnemonic);
    const requestData = {
      asset_name: Buffer.from('WST', 'utf8').toString('hex'), // Convert "WST" to hex
      issuer: mintAccount.address,
      quantity: mintTokensAmount,
    };

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
      await signAndSentTx(lucid, tx);
    } catch (error) {
      console.error('Minting failed:', error);
    }
  };

  const onSend = async () => {
    lucid.selectWallet.fromSeed(mintAccount.mnemonic);
    console.log('send tokens');
    const requestData = {
      asset_name: Buffer.from('WST', 'utf8').toString('hex'), // Convert "WST" to hex
      issuer: mintAccount.address,
      quantity: sendTokensAmount,
      recipient: sendRecipientAddress,
      sender: mintAccount.address,
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
      await signAndSentTx(lucid, tx);
      const newAccountBalance = await getWalletBalance(sendRecipientAddress);
      const recipientWalletKey = (Object.keys(accounts) as (keyof Accounts)[]).find(
        (key) => accounts[key].address === sendRecipientAddress
      );
      if (recipientWalletKey) {
        changeWalletAccountDetails(recipientWalletKey, {
          ...accounts[recipientWalletKey],
          balance: newAccountBalance,
        });
      }
      setAlertStatus(true);
    } catch (error) {
      console.error('Send failed:', error);
    }
  };

  const onFreeze = async () => {
    console.log('freeze an account');
    lucid.selectWallet.fromSeed(mintAccount.mnemonic);
    const requestData = {
      issuer: mintAccount.address,
      blacklist_address: freezeAccountNumber,
    };
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
      await signAndSentTx(lucid, tx);
      const frozenWalletKey = (Object.keys(accounts) as (keyof Accounts)[]).find(
        (key) => accounts[key].address === freezeAccountNumber
      );
      if (frozenWalletKey) {
        changeWalletAccountDetails(frozenWalletKey, {
          ...accounts[frozenWalletKey],
          status: 'Frozen',
        });
      }
    } catch (error) {
      console.error('Freeze failed:', error);
    }
  };

  const onSeize = async () => {
    console.log('seize account funds');
    lucid.selectWallet.fromSeed(mintAccount.mnemonic);
    const requestData = {
      issuer: mintAccount.address,
      target: seizeAccountNumber,
    };
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
      // const tx = await lucid.fromTx(response.data.cborHex);
      // await signAndSentTx(lucid, tx);
      // const newAccountBalance = await getWalletBalance(seizeAccountNumber);
      // const seizeWalletKey = (Object.keys(accounts) as (keyof Accounts)[]).find(
      //   (key) => accounts[key].address === seizeAccountNumber
      // );
      // if (seizeWalletKey) {
      //   changeWalletAccountDetails(seizeWalletKey, {
      //     ...accounts[seizeWalletKey],
      //     balance: newAccountBalance,
      //   });
      // }
    } catch (error) {
      console.error('Seize failed:', error);
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
  label="Account Number"
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

const seizeContent =  <Box>
<WSTTextField 
value={seizeAccountNumber}
onChange={(e) => setSeizeAccountNumber(e.target.value)}
label="Account Number"
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
      value={mintAccount.address}
      fullWidth={true}
      label="Your Address"
      />
  </Box>;

  const getContentComponent = () => {
    switch (selectedTab) {
      case 'Mint Actions':
        return <>
          <Box sx={{display: 'flex', justifyContent: 'space-between', alignItems: 'end', marginBottom: '16px'}}>
          <Box>
            <Typography variant='h4'>Mint Balance</Typography>
            <Typography variant='h1'>{mintAccount.balance} WST</Typography>
          </Box>
          <Typography variant='h5'>UserID: {mintAccount.address.slice(0,15)}</Typography>
          </Box>
          <div className="cardWrapperParent">
            <WalletCard tabs={[
          {
            label: 'Mint',
            content: mintContent,
            buttonLabel: 'Mint',
            onAction: onMint
          },
          {
            label: 'Freeze',
            content: freezeContent,
            buttonLabel: 'Freeze',
            onAction: onFreeze
          },
          {
            label: 'Seize',
            content: seizeContent,
            buttonLabel: 'Seize',
            onAction: onSeize
          }
        ]}></WalletCard>
            <WalletCard tabs={[
              {
                label: 'Send',
                content: sendContent,
                buttonLabel: 'Send',
                onAction: onSend
              },
              {
                label: 'Receive',
                content: receiveContent
              }
            ]}/>
          </div>
        </>;
      case 'Accounts':
        return <>
          <Box sx={{marginBottom: '16px'}}>
            <Typography variant='h1'>User Accounts</Typography>
          </Box> 
          <WSTTable />
        </>;
    }
  };

  return (
    <div className="page">
      {getContentComponent()}
      <AlertBar severity={errorMessage ? 'error' : 'success'} message={errorMessage? 'This is an error message showing there was a failed attempt to send WST to a frozen account.' : 'Transaction sent successfully!'}/>
    </div>
  );
}
