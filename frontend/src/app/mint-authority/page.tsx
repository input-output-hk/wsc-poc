'use client';
//React imports
import React, { useEffect, useState } from 'react';

//Axios imports
import axios from 'axios';

//Lucid imports
import { CML, makeTxSignBuilder, paymentCredentialOf } from '@lucid-evolution/lucid';
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
import WSTTable from '../components/WSTTable';
import { getWalletBalance, signAndSentTx, getBlacklist } from '../utils/walletUtils';



export default function Home() {
  const { lucid, mintAccount, accounts, selectedTab, changeAlertInfo, changeMintAccountDetails, changeWalletAccountDetails } = useStore();
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
  
  useEffect(() => {
    const initialize = async () => {
      await fetchUserDetails();
      await fetchBlacklistStatus();
    };
    initialize();
  }, []);

  const fetchUserDetails = async () => {
    const mintBalance = await getWalletBalance(mintAccount.address);
    const userABalance = await getWalletBalance(accounts.alice.address);
    const userBBalance = await getWalletBalance(accounts.bob.address);

    // Update Zustand store with the initialized wallet information
    await changeMintAccountDetails({ ...mintAccount, balance: mintBalance});
    await changeWalletAccountDetails('alice', { ...accounts.alice, balance: userABalance});
    await changeWalletAccountDetails('bob', { ...accounts.bob, balance: userBBalance});
  };

  const fetchBlacklistStatus = async () => {
    const blacklist = await getBlacklist();
    const { accounts, changeWalletAccountDetails } = useStore.getState();
    
    Object.entries(accounts).map(async ([key, account]) => {
      if (!account.address || account.address.trim() === "") {
        // console.log(`${key} has no address yet, skipping`);
        return;
      }
      const credential : LucidCredential = await paymentCredentialOf(account.address);
      if(blacklist.includes(credential.hash)) {
        // console.log('a match was found', key as keyof typeof accounts);
        changeWalletAccountDetails(key as keyof typeof accounts, { ...account, status: 'Frozen',});
      }
    });
  };

  const handleAddressClearedChange = (event: { target: { checked: boolean | ((prevState: boolean) => boolean); }; }) => {
    setAddressCleared(event.target.checked);
  };

  const onMint = async () => {
    if (addressCleared === false) {
      // setAlertStatus(true);
      console.error("Recipient Address not cleared.");
      return;
    }
    changeAlertInfo({severity: 'info', message: 'Processing Mint Request', open: true, link: ''});
    lucid.selectWallet.fromSeed(mintAccount.mnemonic);
    const requestData = {
      asset_name: Buffer.from('WST', 'utf8').toString('hex'), // Convert "WST" to hex
      issuer: mintAccount.address,
      quantity: mintTokensAmount,
      recipient: mintRecipientAddress
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
      // await signAndSentTx(lucid, tx);
      const txBuilder = await makeTxSignBuilder(lucid.wallet(), tx.toTransaction()).complete();
      const cmlTx = txBuilder.toTransaction()
      // console.log("TxBody: " + cmlTx.body().to_json());
      const witnessSet = txBuilder.toTransaction().witness_set();
      const expectedScriptDataHash : CML.ScriptDataHash | undefined = CML.calc_script_data_hash(witnessSet.redeemers()!, CML.PlutusDataList.new(), lucid.config().costModels!, witnessSet.languages());
      // console.log('Calculated Script Data Hash:', expectedScriptDataHash?.to_hex());
      const cmlTxBodyClone = CML.TransactionBody.from_cbor_hex(cmlTx!.body().to_cbor_hex());
      // console.log("TxBody: " + cmlTxBodyClone.to_json());
      const txIDinAlert = await cmlTxBodyClone.to_json();
      const txIDObject = JSON.parse(txIDinAlert);      
      // console.log('Preclone script hash:', cmlTxBodyClone.script_data_hash()?.to_hex());
      cmlTxBodyClone.set_script_data_hash(expectedScriptDataHash!);
      // console.log('Postclone script hash:', cmlTxBodyClone.script_data_hash()?.to_hex());
      const cmlClonedTx = CML.Transaction.new(cmlTxBodyClone, cmlTx!.witness_set(), true, cmlTx!.auxiliary_data());
      const cmlClonedSignedTx = await makeTxSignBuilder(lucid.wallet(), cmlClonedTx).sign.withWallet().complete();

      const txId = await cmlClonedSignedTx.submit();
      await lucid.awaitTx(txId);
      
      changeAlertInfo({severity: 'success', message: 'Successful new WST mint. View the transaction here:', open: true, link: `https://preview.cardanoscan.io/transaction/${txIDObject.inputs[0].transaction_id}`});
      
      
      await fetchUserDetails();
    } catch (error) {
      console.error('Minting failed:', error);
    }
  };

  const onSend = async () => {
    lucid.selectWallet.fromSeed(mintAccount.mnemonic);
    console.log('send tokens');
    changeAlertInfo({severity: 'info', message: 'Transaction processing', open: true, link: ''});
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
      const txId = await signAndSentTx(lucid, tx);
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
      changeAlertInfo({severity: 'success', message: 'Transaction sent successfully!', open: true, link: `https://preview.cardanoscan.io/transaction/${txId.inputs[0].transaction_id}`});
      await fetchUserDetails();
    } catch (error) {
      console.error('Send failed:', error);
    }
  };

  const onFreeze = async () => {
    console.log('freeze an address');
    lucid.selectWallet.fromSeed(mintAccount.mnemonic);
    changeAlertInfo({severity: 'info', message: 'Freeze request processing', open: true, link: ''});
    const requestData = {
      issuer: mintAccount.address,
      blacklist_address: freezeAccountNumber,
      reason: freezeReason,
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
      const txId = await signAndSentTx(lucid, tx);
      changeAlertInfo({severity: 'success', message: 'Address successfully frozen', open: true, link: `https://preview.cardanoscan.io/transaction/${txId.inputs[0].transaction_id}`});
      const frozenWalletKey = (Object.keys(accounts) as (keyof Accounts)[]).find(
        (key) => accounts[key].address === freezeAccountNumber
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
    }
  };

  const onUnfreeze = async () => {
    console.log('unfreeze an account');
    lucid.selectWallet.fromSeed(mintAccount.mnemonic);
    changeAlertInfo({severity: 'info', message: 'Unfreeze request processing', open: true, link: ''});
    const requestData = {
      issuer: mintAccount.address,
      blacklist_address: unfreezeAccountNumber,
      reason: "(unfreeze)"
    };
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
      changeAlertInfo({severity: 'success', message: 'Address successfully unfrozen', open: true, link: `https://preview.cardanoscan.io/transaction/${txId.inputs[0].transaction_id}`});
      const unfrozenWalletKey = (Object.keys(accounts) as (keyof Accounts)[]).find(
        (key) => accounts[key].address === freezeAccountNumber
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
    }
  };

  const onSeize = async () => {
    console.log('seize account funds');
    lucid.selectWallet.fromSeed(mintAccount.mnemonic);
    changeAlertInfo({severity: 'info', message: 'WST seizure processing', open: true, link: ''});
    const requestData = {
      issuer: mintAccount.address,
      target: seizeAccountNumber,
      reason: seizeReason,
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
      const tx = await lucid.fromTx(response.data.cborHex);
      const txId = await signAndSentTx(lucid, tx);
      const newAccountBalance = await getWalletBalance(seizeAccountNumber);
      const seizeWalletKey = (Object.keys(accounts) as (keyof Accounts)[]).find(
        (key) => accounts[key].address === seizeAccountNumber
      );
      if (seizeWalletKey) {
        changeWalletAccountDetails(seizeWalletKey, {
          ...accounts[seizeWalletKey],
          balance: newAccountBalance,
        });
      }
      changeAlertInfo({severity: 'success', message: 'Funds successfully seized', open: true, link: `https://preview.cardanoscan.io/transaction/${txId.inputs[0].transaction_id}`});
      await fetchUserDetails();
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
            <Typography variant='h4'>Mint Authority Balance</Typography>
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
            label: 'Unfreeze',
            content: unFreezeContent,
            buttonLabel: 'Unfreeze',
            onAction: onUnfreeze
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
      case 'Addresses':
        return <>
          <Box sx={{marginBottom: '16px'}}>
            <Typography variant='h1'>Addresses</Typography>
          </Box> 
          <WSTTable />
        </>;
    }
  };

  return (
    <div className="page">
      {getContentComponent()}
    </div>
  );
}


