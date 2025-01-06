'use client';
//React imports
import React, { useState } from 'react';

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
  const { selectedTab, errorMessage, setAlertStatus } = useStore();
  const mintNavOpt = ['Mint', 'Freeze'];
  const walletNavOpt = ['Send', 'Receive'];
  const walletBttnLabel = ['Send'];
  const [addressCleared, setAddressCleared] = useState(false);

  const handleAddressClearedChange = (event: { target: { checked: boolean | ((prevState: boolean) => boolean); }; }) => {
    setAddressCleared(event.target.checked);
  };

  const onMint = () => {
    console.log('mint a token');
  };

  const onFreeze = () => {
    console.log('freeze an account');
  };

  const onSend = () => {
    console.log('send tokens');
    setAlertStatus(true);
  };

  const mintContent =  <Box>
  <WSTTextField 
      defaultValue={36} 
      label="Number of Tokens to Mint"
      fullWidth={true}
  />
  <WSTTextField 
      defaultValue='addr_sdfah35gd808xxx' 
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
  defaultValue='addr_sdfah35gd808xxx'
  label="Account Number"
  fullWidth={true}
  />
  </Box>
  
  const sendContent =  <Box>
  <WSTTextField 
      defaultValue={36} 
      label="Number of Tokens to Mint"
      fullWidth={true}
  />
  <WSTTextField 
      defaultValue='addr_sdfah35gd808xxx' 
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
            <WalletCard tabLabels={mintNavOpt} cardContentSection1={mintContent} cardContentSection2={freezeContent} bttnLabels={mintNavOpt} onAction1={onMint} onAction2={onFreeze}/>
            <WalletCard tabLabels={walletNavOpt} cardContentSection1={sendContent} cardContentSection2={receiveContent} bttnLabels={walletBttnLabel} onAction1={onSend}/>
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
          <WalletCard tabLabels={walletNavOpt} cardContentSection1={sendContent} cardContentSection2={receiveContent} bttnLabels={walletBttnLabel} onAction1={onSend}/>
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
