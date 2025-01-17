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

//Local Imports
import useStore from '../store/store'; 
import { useEffect, useState } from "react";

const progLogicBase : LucidCredential = {
  type: "Script",
  hash: "fca77bcce1e5e73c97a0bfa8c90f7cd2faff6fd6ed5b6fec1c04eefa"
}

const stableCoin : Unit = toUnit("b34a184f1f2871aa4d33544caecefef5242025f45c3fa5213d7662a9", "575354")


export default function WSTTable() {
  const [uniqueAddresses, setUniqueAddresses] = useState<string[]>([]);
  const [balanceMap, setBalanceMap] = useState<Map<Address, number>>(new Map());
  const { lucid, accounts } = useStore();
  const accountArray = Object.values(accounts);

  const getAccounts = async () => {
    const progUTxOs : UTxO[] = await lucid.utxosAtWithUnit(progLogicBase, stableCoin);
    const addresses = new Set<string>();
    const valueMap = new Map<Address, number>();
    progUTxOs.forEach(utxo => {
      addresses.add(utxo.address)
      valueMap.set(utxo.address, Number(utxo.assets[stableCoin]))
    });
    setBalanceMap(valueMap);
    setUniqueAddresses(Array.from(addresses));
  }
  
  useEffect(() => {
    getAccounts();
  }, []);


  return (
    <Box className="tableContainerBox">
    <TableContainer component={Paper}>
      <Table size="small" aria-label="simple table" stickyHeader>
      <TableHead>
        <TableRow>
          <TableCell>Address</TableCell>
          <TableCell>Account Status</TableCell>
          <TableCell align="right">Account Balance</TableCell>
        </TableRow>
      </TableHead>
      <TableBody>
        {
          accountArray.filter((acct) => acct.address !== "").map((acct, i) => (
            <TableRow key={i}>
              <TableCell>
                  {`${acct?.address.slice(0,15)}...${acct?.address.slice(104,108)}`}
              </TableCell>
              <TableCell sx={{color: acct.status === 'Frozen' ? 'error.main' : 'success.main', fontWeight: '500'}}>
                  {acct.status}
              </TableCell>
              <TableCell align="right">
                  {`${acct?.balance} WST`}
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
