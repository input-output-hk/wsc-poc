'use client'
//Lucid imports
import type { Address, Credential as LucidCredential, Unit, UTxO } from "@lucid-evolution/core-types";
import { toUnit } from "@lucid-evolution/lucid";

//Mui imports
import { Box } from "@mui/material";
import TableContainer from "@mui/material/TableContainer";
import Table from "@mui/material/Table";
import TableBody from "@mui/material/TableBody";
import TableCell from "@mui/material/TableCell";
import TableHead from "@mui/material/TableHead";
import TableRow from "@mui/material/TableRow";
import Paper from "@mui/material/Paper";
import ContentCopyIcon from '@mui/icons-material/ContentCopy';

//Local Imports
import useStore from '../store/store'; 
import { useContext, useEffect } from "react";
import IconButton from './WSTIconButton';
import DemoEnvironmentContext from "../context/demoEnvironmentContext";



export default function WSTTable() {
  const { lucid, accounts } = useStore();
  const accountArray = Object.values(accounts);
  const demoEnv = useContext(DemoEnvironmentContext);
  const stableCoin : Unit = toUnit(demoEnv.minting_policy, demoEnv.token_name);
  
  const progLogicBase : LucidCredential = {
    type: "Script",
    hash: demoEnv.prog_logic_base_hash
  }

  const getAccounts = async () => {
    const progUTxOs : UTxO[] = await lucid.utxosAtWithUnit(progLogicBase, stableCoin);
    const addresses = new Set<string>();
    const valueMap = new Map<Address, number>();
    progUTxOs.forEach(utxo => {
      addresses.add(utxo.address)
      valueMap.set(utxo.address, Number(utxo.assets[stableCoin]))
    });
  }
  
  useEffect(() => {
    getAccounts();
  }, []);

  const copyToClipboard = (str: string) => {
    navigator.clipboard.writeText(str);
  }

  return (
    <Box className="tableContainerBox">
    <TableContainer component={Paper}>
      <Table size="small" aria-label="simple table" stickyHeader>
      <TableHead>
        <TableRow>
          <TableCell>Regular Address</TableCell>
          <TableCell>Programmable Address</TableCell>
          <TableCell>Address Status</TableCell>
          <TableCell align="right">Address Balance</TableCell>
        </TableRow>
      </TableHead>
      <TableBody>
        {
          accountArray.filter((acct) => acct.regular_address !== "").map((acct, i) => (
            <TableRow key={i}>
              <TableCell>
                  {`${acct?.regular_address.slice(0,15)}...${acct?.regular_address.slice(104,108)}`}
                  <IconButton onClick={() => copyToClipboard(acct.regular_address)} icon={<ContentCopyIcon />}/>
              </TableCell>
              <TableCell>
                  {`${acct?.programmable_token_address.slice(0,15)}...${acct?.programmable_token_address.slice(104,108)}`}
                  <IconButton onClick={() => copyToClipboard(acct.programmable_token_address)} icon={<ContentCopyIcon />}/>
              </TableCell>
              <TableCell sx={{color: acct.status === 'Frozen' ? 'error.main' : 'success.main', fontWeight: '500'}}>
                  {acct.status}
              </TableCell>
              <TableCell align="right">
                  {`${acct?.balance.wst} WST`}
              </TableCell>
            </TableRow>
          ))
        }
        {/* {[...uniqueAddresses].map((address, index) => (
            <TableRow key={index}>
              <TableCell>{`${address.slice(0,15)}...${address.slice(104,108)}`}</TableCell>
              <TableCell sx={{color: 'success.main', fontWeight: '500'}}>
                Active
              </TableCell>
              <TableCell align="right">
                {`${balanceMap.get(address)} WST`}
              </TableCell>
            </TableRow>
          ))} */}
      </TableBody>    
    </Table>
    </TableContainer>
    </Box>
  );
}
