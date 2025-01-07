'use client';
//React imports
import React, { useState } from 'react';
import axios from 'axios';

//Mui imports
import { Box, Typography } from '@mui/material';
import Checkbox from '@mui/material/Checkbox';
import FormControlLabel from '@mui/material/FormControlLabel';

//Local components
import useStore from './store/store'; 
import WalletCard from './components/Card';
import WSTTextField from './components/WSTTextField';
import CopyTextField from './components/CopyTextField';
import WSTTable from './components/WSTTable';
import AlertBar from './components/AlertBar';

export default function Home() {
  const { mintAccount, selectedTab, errorMessage, setAlertStatus } = useStore();
  const [addressCleared, setAddressCleared] = useState(false);

  // temp state for each text field
  const [mintTokens, setMintTokens] = useState(36);
  const [recipientAddress, setRecipientAddress] = useState('addr_sdfah35gd808xxx');
  const [accountNumber, setAccountNumber] = useState('addr_sdfah35gd808xxx');
  const [reason, setReason] = useState('Enter reason here');

  const handleAddressClearedChange = (event: { target: { checked: boolean | ((prevState: boolean) => boolean); }; }) => {
    setAddressCleared(event.target.checked);
  };

  const onMint = async () => {
    const requestData = {
      asset_name: 'WST',
      issuer: mintAccount,
      amount: mintTokens,
    };

    try {
      const response = await axios.post(
        'http://localhost:9000/api/tx/programmable-token/issue', 
        requestData, 
        {
          headers: {
            'Content-Type': 'application/json;charset=utf-8', 
          },
        }
      );
  
      console.log('Mint response:', response.data);
    } catch (error) {
      console.error('Minting failed:', error);
    }
  };

  const onFreeze = () => {
    console.log('freeze an account');
  };

  const onSeize = () => {
    console.log('freeze an account');
  };

  const onSend = () => {
    console.log('send tokens');
    setAlertStatus(true);
  };

  const mintContent =  <Box>
  <WSTTextField 
      value={mintTokens}
      onChange={(e) => setMintTokens(Number(e.target.value))}
      label="Number of Tokens to Mint"
      fullWidth={true}
  />
  <WSTTextField 
      value={recipientAddress}
      onChange={(e) => setRecipientAddress(e.target.value)}
      label="Recipient’s Address"
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
  value={accountNumber}
  onChange={(e) => setAccountNumber(e.target.value)}
  label="Account Number"
  fullWidth={true}
  />
  <WSTTextField 
  value={reason}
  onChange={(e) => setReason(e.target.value)}
  label="Reason"
  fullWidth={true}
  multiline={true}
  minRows={2}
  maxRows={3}
  />
  </Box>

const seizeContent =  <Box>
<WSTTextField 
value={accountNumber}
onChange={(e) => setAccountNumber(e.target.value)}
label="Account Number"
fullWidth={true}
/>
<WSTTextField 
value={reason}
onChange={(e) => setReason(e.target.value)}
label="Reason"
fullWidth={true}
multiline={true}
minRows={2}
maxRows={3}
/>
</Box>
  
  const sendContent =  <Box>
  <WSTTextField 
      value={mintTokens}
      onChange={(e) => setMintTokens(Number(e.target.value))}
      label="Number of Tokens to Mint"
      fullWidth={true}
  />
  <WSTTextField 
      value={recipientAddress}
      onChange={(e) => setRecipientAddress(e.target.value)}
      label="Recipient’s Address"
      fullWidth={true}
  />
  </Box>;

  const receiveContent =  <Box>
    <CopyTextField 
      value="addr_sdfah35gd808xxx"
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
            <Typography variant='h1'>1,000,000 WST</Typography>
          </Box>
          <Typography variant='h5'>UserID: xxxxxxx7850</Typography>
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
      case 'Wallet':
        return <>
        <Box sx={{display: 'flex', justifyContent: 'space-between', alignItems: 'end', marginBottom: '16px'}}>
        <Box>
          <Typography variant='h4'>Account Balance</Typography>
          <Typography variant='h1'>3,478 WST</Typography>
        </Box>
        <Typography variant='h5'>UserID: xxxxxxx7850</Typography>
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
