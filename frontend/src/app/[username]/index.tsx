'use client';
//React imports
import React, { useContext, useEffect, useState } from 'react';

//Axios imports
import axios from 'axios';

//Mui imports
import { Box, Checkbox, FormControlLabel, Typography } from '@mui/material';

//Local components
import useStore from '../store/store'; 
import { Accounts } from '../store/types';
import { getWalletBalance, signAndSentTx } from '../utils/walletUtils';
import WalletCard from '../components/Card';
import WSTTextField from '../components/WSTTextField';
import CopyTextField from '../components/CopyTextField';
import DemoEnvironmentContext from '../context/demoEnvironmentContext';

export default function Profile() {
  const { lucid, currentUser, mintAccount, changeAlertInfo, changeWalletAccountDetails } = useStore();
  const accounts = useStore((state) => state.accounts);
  const [overrideTx, setOverrideTx] = useState<boolean>(false);
  const demoEnv = useContext(DemoEnvironmentContext);

  useEffect(() => {
    useStore.getState();
    // console.log("accounts changed:", accounts);
  }, [accounts]);

  const getUserAccountDetails = () => {
    switch (currentUser) {
      case "Alice": return accounts.alice;
      case "Bob": return accounts.bob;
      case "Connected Wallet": return accounts.walletUser;
    };
  };

  const getUserMnemonic = () => {
    switch (currentUser) {
      case "Alice": return demoEnv.user_a;
      case "Bob": return demoEnv.user_b;
      case "Mint Authority": return demoEnv.mint_authority;
      case "Connected Wallet": return ""; //TODO: this seems to be broken
    };

  }

  // temp state for each text field
  const [sendTokenAmount, setMintTokens] = useState(0);
  const [sendRecipientAddress, setsendRecipientAddress] = useState('address');

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
    lucid.selectWallet.fromSeed(getUserMnemonic());
    const requestData = {
      asset_name: Buffer.from('WST', 'utf8').toString('hex'), // Convert "WST" to hex
      issuer: mintAccount.address,
      quantity: sendTokenAmount,
      recipient: sendRecipientAddress,
      sender: accountInfo.address,
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
      await updateAccountBalance(accountInfo.address);
      changeAlertInfo({severity: 'success', message: 'Transaction sent successfully!', open: true, link: `${demoEnv.explorer_url}/${txId}`});
    } catch (error) {
      console.error('Send failed:', error);
    }
  };

  const updateAccountBalance = async (address: string) => {
    const newAccountBalance = await getWalletBalance(demoEnv, address, lucid);
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
    <FormControlLabel
      control={<Checkbox size="small" checked={overrideTx} onChange={x => setOverrideTx(x.target.checked)} />}
      label="⚠️ Force send failing transaction"
      sx={{ mb: 2 }}
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
          <Typography variant='h4'>Address Balance</Typography>
          <Typography variant='h1'>{getUserAccountDetails()?.balance.wst} WST</Typography>
          <Typography variant='h5'>{getUserAccountDetails()?.balance.ada} Ada</Typography>
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
