
'use client'
//React Imports
import * as React from 'react';

//Next.js Imports
import { useRouter } from 'next/navigation';

//MUI Imports
import Button from '@mui/material/Button';
import Menu from '@mui/material/Menu';
import MenuItem from '@mui/material/MenuItem';
import KeyboardArrowDownIcon from '@mui/icons-material/KeyboardArrowDown';

//Local Imports
import useStore from '../store/store'; 
import { UserName } from '../store/types';
import { selectLucidWallet, getWalletBalance, getProgrammableTokenAddress, areStakeScriptsRegistered, WalletType } from '../utils/walletUtils';
import DemoEnvironmentContext from '../context/demoEnvironmentContext';

export default function ProfileSwitcher() {
  const [anchorEl, setAnchorEl] = React.useState<HTMLElement | null>(null);
  const { currentUser, accounts, changeWalletAccountDetails, changeAlertInfo } = useStore();
  const lucid = useStore(state => state.lucid);
  const changeUserAccount = useStore(state => state.changeUserAccount);
  const router = useRouter();
  const demoContext = React.useContext(DemoEnvironmentContext);

  React.useEffect(() => {
    if (currentUser === 'Not Connected') {
      return;
    }
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

  const handleWalletConnect = async (walletType: WalletType) => {
    try {
      const cardanoApi = typeof window !== 'undefined' ? (window as any)?.cardano : undefined;
      if (!lucid || !cardanoApi || !cardanoApi[walletType.toLowerCase()]) {
        throw new Error(`${walletType} wallet is not available in this browser.`);
      }
      await selectLucidWallet(lucid, walletType);
      const userAddress = await lucid.wallet().address();
      const [userBalance, programmableAddress, hasRegisteredScripts] = await Promise.all([
        getWalletBalance(demoContext, userAddress),
        getProgrammableTokenAddress(userAddress),
        areStakeScriptsRegistered(demoContext, lucid, userAddress),
      ]);
      changeWalletAccountDetails('walletUser', {
        ...accounts.walletUser,
        regular_address: userAddress,
        programmable_token_address: programmableAddress,
        balance: userBalance,
        hasRegisteredScripts,
      });
      changeUserAccount('Connected Wallet');
      router.push('/connected-wallet');
    } catch (error) {
      console.error(`Failed to connect ${walletType}`, error);
      changeAlertInfo({
        severity: 'error',
        message: `Unable to connect to ${walletType}. Please ensure the wallet extension is installed and unlocked.`,
        open: true,
        link: '',
      });
    } finally {
      handleClose();
    }
  };

  const buttonLabel = currentUser === 'Connected Wallet' ? 'Connected Wallet' : 'Connect Wallet';

  return (
    <>
      <Button
        variant="contained"
        color="primary"
        onClick={handleClick}
        endIcon={<KeyboardArrowDownIcon />}
      >
        {buttonLabel}
      </Button>
      <Menu
        anchorEl={anchorEl}
        open={Boolean(anchorEl)}
        onClose={handleClose}
      >
        <MenuItem onClick={() => handleWalletConnect('Lace')}>Lace</MenuItem>
        <MenuItem onClick={() => handleWalletConnect('Eternl')}>Eternl</MenuItem>
        <MenuItem onClick={() => handleWalletConnect('Yoroi')}>Yoroi</MenuItem>
      </Menu>
    </>
  );
}
