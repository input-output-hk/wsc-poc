"use client";
//React imports 
import React, { useEffect } from "react";

//Mui imports
import { AppRouterCacheProvider } from '@mui/material-nextjs/v15-appRouter';
import NavDrawer from './components/NavDrawer';

//Local file
import { ThemeModeProvider } from "./styles/themeContext";
import "./styles/globals.css";
import { makeLucid, getWalletFromSeed, getWalletBalance } from "./utils/walletUtils";
import useStore from './store/store'; 
import WSTAppBar from "./components/WSTAppBar";
import AlertBar from './components/AlertBar';

export default function ClientLayout({ children }: { children: React.ReactNode }) {
    const { mintAccount, accounts, alertInfo, changeMintAccountDetails, changeWalletAccountDetails, setLucidInstance } = useStore();

    useEffect(() => {
        const fetchUserWallets = async () => {
          try {
            // retrieve wallet info
            const mintAuthorityWallet = await getWalletFromSeed(mintAccount.mnemonic);
            const walletA = await getWalletFromSeed(accounts.userA.mnemonic);
            
            const walletB = await getWalletFromSeed(accounts.userB.mnemonic);
    
            const mintStartBalance = await getWalletBalance(mintAuthorityWallet.address);
            const userAStartBalance = await getWalletBalance(walletA.address);
            const userBStartBalance = await getWalletBalance(walletB.address);
            // Update Zustand store with the initialized wallet information
            changeMintAccountDetails({ ...mintAccount, address: mintAuthorityWallet.address, balance: mintStartBalance});
            changeWalletAccountDetails('userA', { ...accounts.userA, address: walletA.address, balance: userAStartBalance});
            changeWalletAccountDetails('userB', { ...accounts.userB, address: walletB.address, balance: userBStartBalance});
    
            const initialLucid = await makeLucid();
            setLucidInstance(initialLucid);
            console.log('Wallets initialized');
          } catch (error) {
            console.error('Error initializing wallets:', error);
          }
        };
    
        fetchUserWallets();
      },[]);

  if(accounts.userB.address === '') {
    return <div className="mainLoadingContainer">
    <div className="mainLoader" />
  </div>;
  };

  return (
    <AppRouterCacheProvider>
      <ThemeModeProvider>
        <main>
          <WSTAppBar />
          <NavDrawer />
          <div className="contentSection">{children}</div>
          <AlertBar/>
        </main>
      </ThemeModeProvider>
    </AppRouterCacheProvider>
  );
}
