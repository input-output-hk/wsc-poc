'use client';
//React imports
import React, { useState } from 'react';

//Axios imports
import axios from 'axios';

//Mui imports
import { Box, Typography } from '@mui/material';
import Checkbox from '@mui/material/Checkbox';
import FormControlLabel from '@mui/material/FormControlLabel';

//Lucid imports
import { CML, makeTxSignBuilder } from "@lucid-evolution/lucid";

//Local components
import useStore from '../store/store'; 
import WalletCard from '../components/Card';
import WSTTextField from '../components/WSTTextField';
import CopyTextField from '../components/CopyTextField';
import WSTTable from '../components/WSTTable';
import AlertBar from '../components/AlertBar';
import { adjustMintOutput, deriveProgrammableAddress } from '../utils/walletUtils';

export default function Home() {
  const { lucid, mintAccount, selectedTab, errorMessage, setAlertStatus } = useStore();
  const [addressCleared, setAddressCleared] = useState(false);
  // Temporary state for each text field
  const [mintTokens, setMintTokens] = useState(36);
  const [recipientAddress, setRecipientAddress] = useState('addr_test1qpynxme7c0tcmmvgk2tjuv63aw7zk9tk6yqkaqd48ulhkyl5f6v47dp5rc7286z5f57339d0c79khw4y3lwxzm8ywkzsvudp69');
  const [accountNumber, setAccountNumber] = useState('addr_test1qpynxme7c0tcmmvgk2tjuv63aw7zk9tk6yqkaqd48ulhkyl5f6v47dp5rc7286z5f57339d0c79khw4y3lwxzm8ywkzsvudp69');
  const [reason, setReason] = useState('Enter reason here');

  const handleAddressClearedChange = (event: { target: { checked: boolean | ((prevState: boolean) => boolean); }; }) => {
    setAddressCleared(event.target.checked);
  };

  const onMint = async () => {
    const requestData = {
      asset_name: Buffer.from('WST', 'utf8').toString('hex'), // Convert "WST" to hex
      issuer: mintAccount.address,
      quantity: mintTokens,
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
      lucid.selectWallet.fromSeed(mintAccount.mnemonic);
      console.log('Mint response:', response.data);
      const tx = await lucid.fromTx(response.data.cborHex);
      const txBuilder = await makeTxSignBuilder(lucid.wallet(), tx.toTransaction()).complete();
      const cmlTxInternal = txBuilder.toTransaction()
      console.log("TxBody: " + cmlTxInternal.body().to_json());
      const cmlTx = adjustMintOutput(cmlTxInternal, (await deriveProgrammableAddress(lucid, recipientAddress)), BigInt(mintTokens))
      const witnessSet = txBuilder.toTransaction().witness_set();
      const expectedScriptDataHash : CML.ScriptDataHash | undefined = CML.calc_script_data_hash(witnessSet.redeemers()!, CML.PlutusDataList.new(), lucid.config().costModels!, witnessSet.languages());
      console.log('Calculated Script Data Hash:', expectedScriptDataHash?.to_hex());
      const cmlTxBodyClone = CML.TransactionBody.from_cbor_hex(cmlTx!.body().to_cbor_hex());
      console.log('Preclone script hash:', cmlTxBodyClone.script_data_hash()?.to_hex());
      cmlTxBodyClone.set_script_data_hash(expectedScriptDataHash!);
      console.log('Postclone script hash:', cmlTxBodyClone.script_data_hash()?.to_hex());
      const cmlClonedTx = CML.Transaction.new(cmlTxBodyClone, cmlTx!.witness_set(), true, cmlTx!.auxiliary_data());
      const cmlClonedSignedTx = await makeTxSignBuilder(lucid.wallet(), cmlClonedTx).sign.withWallet().complete();

      const txId = await cmlClonedSignedTx.submit();
      await lucid.awaitTx(txId);
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
