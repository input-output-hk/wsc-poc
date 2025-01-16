
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

export default function WSTTable() {
  const { accounts } = useStore();
  const accountArray = Object.values(accounts);

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
        {
          accountArray.filter((acct) => acct.address !== "").map((acct, i) => (
            <TableRow key={i}>
              <TableCell>
                  {acct?.address.slice(0,15)}
              </TableCell>
              <TableCell sx={{color: acct?.status === 'Frozen' ? 'error.main' : 'success.main', fontWeight: '500'}}>
                  {acct?.status}
              </TableCell>
              <TableCell align="right">
                  {`${acct?.balance} WST`}
              </TableCell>
            </TableRow>
          ))
        }
      </TableBody>    
    </Table>
    </TableContainer>
  );
}
