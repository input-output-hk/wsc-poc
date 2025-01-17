'use client';
//React imports
import React, { useEffect, useState } from 'react';

//Axios imports
import axios from 'axios';

//Mui imports
import { Box, Typography } from '@mui/material';

//Local components
import useStore from '../store/store'; 
import { Accounts } from '../store/types';
import { getWalletBalance, signAndSentTx } from '../utils/walletUtils';
import WalletCard from '../components/Card';
import WSTTextField from '../components/WSTTextField';
import CopyTextField from '../components/CopyTextField';

export default function Profile() {
  const { lucid, currentUser, mintAccount, alertInfo, changeAlertInfo, changeWalletAccountDetails } = useStore();
  const accounts = useStore((state) => state.accounts);

  useEffect(() => {
    useStore.getState();
    // console.log("accounts changed:", accounts);
  }, [accounts]);

  const getUserAccountDetails = () => {
    switch (currentUser) {
      case "User A": return accounts.userA;
      case "User B": return accounts.userB;
      case "Connected Wallet": return accounts.walletUser;
    };
  };

  // temp state for each text field
  const [sendTokenAmount, setMintTokens] = useState(0);
  const [sendRecipientAddress, setsendRecipientAddress] = useState('address');

  const onSend = async () => {
    console.log('start sending tokens');
    changeAlertInfo({severity: 'info', message: 'Transaction processing', open: true,});
    const accountInfo = getUserAccountDetails();
    if (!accountInfo) {
      console.error("No valid send account found! Cannot send.");
      return;
    }
    lucid.selectWallet.fromSeed(accountInfo.mnemonic);
    const requestData = {
      asset_name: Buffer.from('WST', 'utf8').toString('hex'), // Convert "WST" to hex
      issuer: mintAccount.address,
      quantity: sendTokenAmount,
      recipient: sendRecipientAddress,
      sender: accountInfo.address,
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
      await updateAccountBalance(sendRecipientAddress);
      await updateAccountBalance(accountInfo.address);
      changeAlertInfo({...alertInfo, open: true,});
    } catch (error: any) {
      if (error.response.data.includes('TransferBlacklistedCredential')) {
        changeAlertInfo({
          severity: 'error',
          message: 'Cannot send WST with frozen account.',
          open: true,
        });
        return;
      } else {
        console.error('Send failed:', error);
      }
    }
  };

  const updateAccountBalance = async (address: string) => {
    const newAccountBalance = await getWalletBalance(address);
      const walletKey = (Object.keys(accounts) as (keyof Accounts)[]).find(
        (key) => accounts[key].address === address
      );
      if (walletKey) {
        changeWalletAccountDetails(walletKey, {
          ...accounts[walletKey],
          balance: newAccountBalance,
        });
      }
  };
  
  const sendContent =  <Box>
  <WSTTextField 
      placeholder='0.0000'
      value={sendTokenAmount}
      onChange={(e) => setMintTokens(Number(e.target.value))}
      label="Number of Tokens to Send"
      fullWidth={true}
  />
  <WSTTextField 
      placeholder="address"
      value={sendRecipientAddress}
      onChange={(e) => setsendRecipientAddress(e.target.value)}
      label="Recipient’s Address"
      fullWidth={true}
  />
  </Box>;

  const receiveContent =  <Box>
    <CopyTextField 
      value={getUserAccountDetails()?.address ?? ''}
      fullWidth={true}
      label="Your Address"
      />
  </Box>;

  return (
    <div className="page">
    <Box sx={{display: 'flex', justifyContent: 'space-between', alignItems: 'end', marginBottom: '16px'}}>
        <Box>
          <Typography variant='h4'>Account Balance</Typography>
          <Typography variant='h1'>{getUserAccountDetails()?.balance} WST</Typography>
        </Box>
        <Typography variant='h5'>{getUserAccountDetails()?.address.slice(0,15)}</Typography>
        </Box>
        <div className="cardWrapperParent">
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
    </div>
  );
}