'use client'
//React Imports
import * as React from 'react';

//Mui imports
import Drawer from '@mui/material/Drawer';
import List from '@mui/material/List';
import ListItem from '@mui/material/ListItem';
import ListItemButton from '@mui/material/ListItemButton';
import ListItemIcon from '@mui/material/ListItemIcon';
import ListItemText from '@mui/material/ListItemText';
import ImportExportIcon from '@mui/icons-material/ImportExport';
import FormatListBulletedIcon from '@mui/icons-material/FormatListBulleted';
import AccountBalanceWalletIcon from '@mui/icons-material/AccountBalanceWallet';
import AppRegistrationIcon from '@mui/icons-material/AppRegistration';

//Local Imports
import useStore from '../store/store'; 
import { MenuTab } from '../store/types';

const drawerWidth = 200;

const iconMapping = {
  'Mint Actions': <ImportExportIcon />,
  'Addresses': <FormatListBulletedIcon />,
  'Wallet': <AccountBalanceWalletIcon />,
  'Register Asset': <AppRegistrationIcon />
};

export default function NavDrawer() {
  const { currentUser, selectTab, selectedTab, accounts } = useStore();

  // Define list items based on the current user
  const connectedWalletTabs: MenuTab[] = ['Wallet', 'Register Asset'];
  if (accounts.walletUser.hasRegisteredScripts) {
    connectedWalletTabs.push('Mint Actions', 'Addresses');
  }

  const listItems: MenuTab[] =
    currentUser === 'Mint Authority'
      ? ['Mint Actions', 'Addresses']
      : currentUser === 'Connected Wallet'
        ? connectedWalletTabs
        : currentUser === 'Not Connected'
          ? []
          : ['Wallet'];

    const handleListItemClick = (item: MenuTab) => {
      selectTab(item); 
    };

  return (
    <>
      <Drawer
        sx={{
          width: drawerWidth,
          flexShrink: 0,
          marginTop: '48px',
          '& .MuiDrawer-paper': {
            width: drawerWidth,
            boxSizing: 'border-box',
            position: 'fixed',
            height: `calc(100% - 48px)`,
            marginTop: '48px',
          },
        }}
        variant="permanent"
        anchor="left"
      >        
        <List>
          {listItems.map((text) => (
            <ListItem key={text} disablePadding>
              <ListItemButton 
                onClick={() => handleListItemClick(text)} 
                disableRipple 
                disableTouchRipple
                selected={selectedTab === text}>
                <ListItemIcon>
                  {iconMapping[text]}
                </ListItemIcon>
                <ListItemText primary={text} />
              </ListItemButton>
            </ListItem>
          ))}
        </List>
      </Drawer>
    </>
  );
}
