//Mui imports
import TableContainer from "@mui/material/TableContainer";
import Table from "@mui/material/Table";
import TableBody from "@mui/material/TableBody";
import TableCell from "@mui/material/TableCell";
import TableHead from "@mui/material/TableHead";
import TableRow from "@mui/material/TableRow";
import Paper from "@mui/material/Paper";
import PanToolOutlinedIcon from '@mui/icons-material/PanToolOutlined';

//Local imports
import WSTCommonButton from "./WSTCommonButton";

const Field = [
    {
        account: 'addr_sdfah35gd808xxx',
        status: 'Frozen',
        balance: 1000
    },
    {
        account: 'addr_sdfah35gd808xxx',
        status: 'Active',
        balance: 1000
    },
    {
        account: 'addr_sdfah35gd808xxx',
        status: 'Active',
        balance: 1000
    },
    {
        account: 'addr_sdfah35gd808xxx',
        status: 'Frozen',
        balance: 1000
    },
    {
        account: 'addr_sdfah35gd808xxx',
        status: 'Active',
        balance: 1000
    }
]

export default function WSTTable() {

  return (
    <TableContainer component={Paper}>
      <Table size="small" aria-label="simple table" stickyHeader>
      <TableHead>
        <TableRow>
          <TableCell>Address</TableCell>
          <TableCell>Account Status</TableCell>
          <TableCell align="right">Account Balance</TableCell>
            <TableCell align="right">Actions</TableCell>
        </TableRow>
      </TableHead>
      <TableBody>
        {Field.map((field, fieldIndex) => (
            <TableRow key={fieldIndex}>
                <TableCell>
                    {field.account}
                </TableCell>
                <TableCell sx={{color: field.status === 'Frozen' ? 'error.main' : 'success.main', fontWeight: '500'}}>
                    {field.status}
                </TableCell>
                <TableCell align="right">
                    {`${field.balance} WST`}
                </TableCell>
                <TableCell align="right">
                    <WSTCommonButton variant='text' size="small" text='Seize Funds' startIcon={<PanToolOutlinedIcon />} />
                </TableCell>
            </TableRow>
        ))}
      </TableBody>    
    </Table>
    </TableContainer>
  );
}