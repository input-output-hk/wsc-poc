
'use client'
//React Imports
import * as React from 'react';

//Next.js Imports
import { useRouter } from 'next/navigation';

//MUI Imports
import Chip from '@mui/material/Chip';
import Menu from '@mui/material/Menu';
import MenuItem from '@mui/material/MenuItem';
import KeyboardArrowDownIcon from '@mui/icons-material/KeyboardArrowDown';

//Local Imports
import useStore from '../store/store'; 
import { UserName } from '../store/types';
import { selectLucidWallet, getWalletBalance } from '../utils/walletUtils';
import DemoEnvironmentContext from '../context/demoEnvironmentContext';

export default function ProfileSwitcher() {
  const [anchorEl, setAnchorEl] = React.useState<HTMLElement | null>(null);
  const { currentUser, accounts, changeWalletAccountDetails } = useStore();
  const lucid = useStore(state => state.lucid);
  const changeUserAccount = useStore(state => state.changeUserAccount);
  const router = useRouter();
  const demoContext = React.useContext(DemoEnvironmentContext);

  React.useEffect(() => {
    // Check the current path and redirect if the currentUser doesn't match
    const expectedPath =
      currentUser === 'Mint Authority'
        ? '/mint-authority'
        : `/${currentUser.toLowerCase().replace(/\s+/g, '-')}`;
    const currentPath = window.location.pathname;

    if (currentPath !== expectedPath) {
      router.push(expectedPath);
    }
  }, [currentUser, router]);

  const handleClick = (event: React.MouseEvent<HTMLDivElement>) => {
    setAnchorEl(event.currentTarget as HTMLElement);
  };

  const handleClose = () => {
    setAnchorEl(null);
  };

  const handleSelect = (user: UserName) => {
    changeUserAccount(user);
   
    // Determine the URL
    const newUrl =
    user === 'Mint Authority'
      ? '/mint-authority'
      : `/${user.toLowerCase().replace(/\s+/g, '-')}`;

    router.push(newUrl);
    handleClose();
  };

  
  const handleWalletConnect = async (user: UserName) => {
    await selectLucidWallet(lucid, "Lace");
    const userAddress = await lucid.wallet().address();
    const userBalance = await getWalletBalance(demoContext, userAddress);
    changeWalletAccountDetails('walletUser', {
      ...accounts.walletUser,
      regular_address: userAddress,
      balance: userBalance,
    });
    handleSelect(user);
  };

  return (
    <>
      <Chip
        label={currentUser}
        onClick={handleClick}
        color="primary"
        deleteIcon={<KeyboardArrowDownIcon />}
        onDelete={handleClick}
      />
      <Menu
        anchorEl={anchorEl}
        open={Boolean(anchorEl)}
        onClose={handleClose}
      >
        <MenuItem onClick={() => handleSelect('Mint Authority')}>Mint Authority</MenuItem>
        <MenuItem onClick={() => handleSelect('Alice')}>Alice</MenuItem>
        <MenuItem onClick={() => handleSelect('Bob')}>Bob</MenuItem>
        <MenuItem onClick={() => handleWalletConnect('Connected Wallet')}>Lace</MenuItem>
      </Menu>
    </>
  );
}
