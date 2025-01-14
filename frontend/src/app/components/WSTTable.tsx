//Mui imports
import TableContainer from "@mui/material/TableContainer";
import Table from "@mui/material/Table";
import TableBody from "@mui/material/TableBody";
import TableCell from "@mui/material/TableCell";
import TableHead from "@mui/material/TableHead";
import TableRow from "@mui/material/TableRow";
import Paper from "@mui/material/Paper";

//Local Imports
import useStore from '../store/store'; 
import { Address, Credential, toUnit, Unit, UTxO } from "@lucid-evolution/lucid";
import { useEffect, useState } from "react";

const progLogicBase : Credential = {
  type: "Script",
  hash: "fca77bcce1e5e73c97a0bfa8c90f7cd2faff6fd6ed5b6fec1c04eefa"
}

const stableCoin : Unit = toUnit("b34a184f1f2871aa4d33544caecefef5242025f45c3fa5213d7662a9", "575354")

export default function WSTTable() {
  const {lucid, userA, userB, } = useStore();
  const [uniqueAddresses, setUniqueAddresses] = useState<string[]>([]);
  const [balanceMap, setBalanceMap] = useState<Map<Address, number>>(new Map());

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
  }, [lucid]);

  useEffect(() => {
    console.log('Unique addresses state updated:', uniqueAddresses);
  }, [uniqueAddresses]);

  return (
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
        <TableRow>
            <TableCell>
                {userA?.address.slice(0,15)}
            </TableCell>
            <TableCell sx={{color: userA?.status === 'Frozen' ? 'error.main' : 'success.main', fontWeight: '500'}}>
                {userA?.status}
            </TableCell>
            <TableCell align="right">
                {`${userA?.balance} WST`}
            </TableCell>
        </TableRow>
        <TableRow>
            <TableCell>
                {userB?.address.slice(0,15)}
            </TableCell>
            <TableCell sx={{color: userB?.status === 'Frozen' ? 'error.main' : 'success.main', fontWeight: '500'}}>
                {userB?.status}
            </TableCell>
            <TableCell align="right">
                {`${userB?.balance} WST`}
            </TableCell>
        </TableRow>
        {[...uniqueAddresses].map((address, index) => (
            <TableRow key={index}>
              <TableCell>{address}</TableCell>
              <TableCell sx={{color: 'success.main', fontWeight: '500'}}>
                Active
              </TableCell>
              <TableCell align="right">
                {`${balanceMap.get(address)} WST`}
              </TableCell>
            </TableRow>
          ))}
      </TableBody>    
    </Table>
    </TableContainer>
  );
}