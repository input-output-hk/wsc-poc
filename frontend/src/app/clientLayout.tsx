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

export default function ClientLayout({ children }: { children: React.ReactNode }) {
    const { mintAccount, userA, userB, changeMintAccountDetails, changeWalletAAccountDetails, changeWalletBAccountDetails, setLucidInstance } = useStore();

    useEffect(() => {
        const fetchUserWallets = async () => {
          try {
            // retrieve wallet info
            const mintAuthorityWallet = await getWalletFromSeed(mintAccount.mnemonic);
            const walletA = await getWalletFromSeed(userA.mnemonic);
            const walletB = await getWalletFromSeed(userB.mnemonic);
    
            const mintStartBalance = await getWalletBalance(mintAuthorityWallet.address);
            // Update Zustand store with the initialized wallet information
            changeMintAccountDetails({ ...mintAccount, address: mintAuthorityWallet.address, balance: mintStartBalance});
            changeWalletAAccountDetails({ ...userA, address: walletA.address});
            changeWalletBAccountDetails({ ...userB, address: walletB.address,});
    
            const initialLucid = await makeLucid();
            setLucidInstance(initialLucid);
            console.log('Wallets initialized');
          } catch (error) {
            console.error('Error initializing wallets:', error);
          }
        };
    
        fetchUserWallets();
      },[]);

  if(userB.address === '') {
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
        </main>
      </ThemeModeProvider>
    </AppRouterCacheProvider>
  );
}
