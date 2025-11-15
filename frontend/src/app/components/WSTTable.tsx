'use client'

import { Box, CircularProgress, Typography } from "@mui/material";
import TableContainer from "@mui/material/TableContainer";
import Table from "@mui/material/Table";
import TableBody from "@mui/material/TableBody";
import TableCell from "@mui/material/TableCell";
import TableHead from "@mui/material/TableHead";
import TableRow from "@mui/material/TableRow";
import Paper from "@mui/material/Paper";
import ContentCopyIcon from '@mui/icons-material/ContentCopy';

import useStore from '../store/store'; 
import IconButton from './WSTIconButton';

export type WSTTableRow = {
  regularAddress: string;
  programmableAddress?: string;
  status?: string;
  balanceText?: string;
  assets?: Array<{ unit: string; quantity: string; assetName?: string }>;
};

interface WSTTableProps {
  rows?: WSTTableRow[];
  loading?: boolean;
  emptyMessage?: string;
}

const formatAddress = (address?: string) => {
  if (!address) return '—';
  if (address.length <= 20) return address;
  return `${address.slice(0, 15)}...${address.slice(-4)}`;
};

const formatAssets = (assets?: Array<{ unit: string; quantity: string; assetName?: string }>) => {
  if (!assets || assets.length === 0) {
    return null;
  }
  return assets
    .map(({ assetName, unit, quantity }) => `${assetName ?? unit}: ${quantity}`)
    .join(', ');
};

export default function WSTTable({ rows, loading = false, emptyMessage = 'No addresses to display.' }: WSTTableProps) {
  const { accounts } = useStore();
  const accountArray = Object.values(accounts);

  const defaultRows: WSTTableRow[] = accountArray
    .filter((acct) => acct.regular_address !== "")
    .map((acct) => ({
      regularAddress: acct.regular_address,
      programmableAddress: acct.programmable_token_address,
      status: acct.status ?? 'Active',
      balanceText: `${acct.balance.wst} WST`,
    }));

  const tableRows = rows ?? defaultRows;

  const copyToClipboard = (str: string) => {
    navigator.clipboard.writeText(str);
  };

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
        {loading ? (
          <TableRow>
            <TableCell colSpan={4} align="center">
              <CircularProgress size={24} />
            </TableCell>
          </TableRow>
        ) : tableRows.length === 0 ? (
          <TableRow>
            <TableCell colSpan={4} align="center">
              <Typography variant="body2" color="text.secondary">
                {emptyMessage}
              </Typography>
            </TableCell>
          </TableRow>
        ) : (
          tableRows.map((row, i) => {
            const balanceDisplay = formatAssets(row.assets) ?? row.balanceText ?? '—';
            return (
              <TableRow key={`${row.regularAddress}-${i}`}>
                <TableCell>
                  {formatAddress(row.regularAddress)}
                  {row.regularAddress && (
                    <IconButton onClick={() => copyToClipboard(row.regularAddress)} icon={<ContentCopyIcon />} />
                  )}
                </TableCell>
                <TableCell>
                  {formatAddress(row.programmableAddress)}
                  {row.programmableAddress && (
                    <IconButton onClick={() => copyToClipboard(row.programmableAddress!)} icon={<ContentCopyIcon />} />
                  )}
                </TableCell>
                <TableCell sx={{ color: row.status === 'Frozen' ? 'error.main' : 'success.main', fontWeight: '500' }}>
                  {row.status ?? 'Active'}
                </TableCell>
                <TableCell align="right">
                  {balanceDisplay}
                </TableCell>
              </TableRow>
            );
          })
        )}
      </TableBody>    
    </Table>
    </TableContainer>
    </Box>
  );
}
