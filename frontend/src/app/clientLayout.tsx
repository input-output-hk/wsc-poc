"use client";
//React imports 
import React, { useEffect } from "react";

//Mui imports
import { AppRouterCacheProvider } from '@mui/material-nextjs/v15-appRouter';
import NavDrawer from './components/NavDrawer';

//Local file
import { ThemeModeProvider } from "./styles/themeContext";
import "./styles/globals.css";
import { initializeMintWallet, createNewWallet } from "./utils/walletUtils";
import useStore from './store/store'; 
import WSTAppBar from "./components/WSTAppBar";

export default function ClientLayout({ children }: { children: React.ReactNode }) {
    const { mintAccount, userA, userB, changeMintAccountDetails, changeWalletAAccountDetails, changeWalletBAccountDetails } = useStore();

    useEffect(() => {
        const fetchKeyAgent = async () => {
          try {
            const agent = await initializeMintWallet(mintAccount.mnemonic);
            const walletA = await createNewWallet();
            const walletB = await createNewWallet();
    
            // Update Zustand store with the initialized wallet information
            changeMintAccountDetails({ ...mintAccount, keyAgent: agent });
            changeWalletAAccountDetails({
                ...userA,
                keyAgent: walletA.agent,
                address: walletA.address,
                mnemonic: walletA.mnemonic,
            });
            changeWalletBAccountDetails({
                ...userB,
                keyAgent: walletB.agent,
                address: walletB.address,
                mnemonic: walletB.mnemonic,
            });
    
            console.log('Wallets initialized');
          } catch (error) {
            console.error('Error initializing KeyAgent:', error);
          }
        };
    
        fetchKeyAgent();
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
