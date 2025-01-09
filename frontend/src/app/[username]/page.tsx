'use client';
//React imports
import React, { useState } from 'react';

//Mui imports
import { Box, Typography } from '@mui/material';

//Local components
import useStore from '../store/store'; 
import WalletCard from '../components/Card';
import WSTTextField from '../components/WSTTextField';
import CopyTextField from '../components/CopyTextField';

export default function Profile() {
  const { setAlertStatus } = useStore();

  // temp state for each text field
  const [mintTokens, setMintTokens] = useState(36);
  const [recipientAddress, setRecipientAddress] = useState('addr_sdfah35gd808xxx');


  const onSend = () => {
    console.log('send tokens');
    setAlertStatus(true);
  };
  
  const sendContent =  <Box>
  <WSTTextField 
      value={mintTokens}
      onChange={(e) => setMintTokens(Number(e.target.value))}
      label="Number of Tokens to Send"
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

  return (
    <div className="page">
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
    </div>
  );
}
