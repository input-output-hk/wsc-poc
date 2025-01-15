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
  const { userA, userB, walletUser } = useStore();

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
          {
            walletUser.address &&
            <TableRow>
              <TableCell>
                  {walletUser?.address.slice(0,15)}
              </TableCell>
              <TableCell sx={{color: userB?.status === 'Frozen' ? 'error.main' : 'success.main', fontWeight: '500'}}>
                  {walletUser?.status}
              </TableCell>
              <TableCell align="right">
                  {`${walletUser?.balance} WST`}
              </TableCell>
            </TableRow>
          }
      </TableBody>    
    </Table>
    </TableContainer>
  );
}