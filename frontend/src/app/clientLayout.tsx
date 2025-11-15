"use client";
//React imports 
import React, { useEffect, useState } from "react";

//Mui imports
import { AppRouterCacheProvider } from '@mui/material-nextjs/v15-appRouter';
import NavDrawer from './components/NavDrawer';

//Local file
import { ThemeModeProvider } from "./styles/themeContext";
import "./styles/globals.css";
import { makeLucid, getWalletFromSeed, getProgrammableTokenAddress } from "./utils/walletUtils";
import useStore from './store/store'; 
import WSTAppBar from "./components/WSTAppBar";
import AlertBar from './components/AlertBar';
import { DemoEnvironment, previewEnv } from "./store/types";
import axios from "axios";
import DemoEnvironmentContext from "./context/demoEnvironmentContext";

async function loadDemoEnvironment(): Promise<DemoEnvironment> {
  const response = await axios.get<DemoEnvironment>("/api/v1/demo-environment",
    {
      headers: {
        'Content-Type': 'application/json;charset=utf-8', 
      },
    });
  return response?.data;
}

export default function ClientLayout({ children }: { children: React.ReactNode }) {
    const { mintAccount, accounts, changeMintAccountDetails, changeWalletAccountDetails, setLucidInstance } = useStore();
    const [demoEnv, setDemoEnv] = useState<DemoEnvironment>(previewEnv);

    useEffect(() => {
        const fetchUserWallets = async () => {
          try {
            const demoEnv = await loadDemoEnvironment();
            console.log("DemoEnvironment:", demoEnv);
            setDemoEnv(demoEnv);

            // retrieve wallet info
            const mintAuthorityWallet = await getWalletFromSeed(demoEnv, demoEnv.mint_authority);
            const walletA = await getWalletFromSeed(demoEnv, demoEnv.user_a);
            const walletB = await getWalletFromSeed(demoEnv, demoEnv.user_b);
            const walletATokenAddr = await getProgrammableTokenAddress(walletA.address);
            const walletBTokenAddr = await getProgrammableTokenAddress(walletB.address);
            const mintAuthorityTokenAddr = await getProgrammableTokenAddress(mintAuthorityWallet.address);
    
            // Update Zustand store with the initialized wallet information
            changeMintAccountDetails({ ...mintAccount, regular_address: mintAuthorityWallet.address, programmable_token_address: mintAuthorityTokenAddr});
            changeWalletAccountDetails('alice', { ...accounts.alice, regular_address: walletA.address, programmable_token_address: walletATokenAddr},);
            changeWalletAccountDetails('bob', { ...accounts.bob, regular_address: walletB.address, programmable_token_address: walletBTokenAddr});
    
            const initialLucid = await makeLucid(demoEnv);
            setLucidInstance(initialLucid);
            console.log('Wallets initialized');
          } catch (error) {
            console.error('Error initializing wallets:', error);
          }
        };
    
        fetchUserWallets();
      },[]);

  if(accounts.bob.regular_address === '') {
    return <div className="mainLoadingContainer">
    <div className="mainLoader" />
  </div>;
  };

  return (
    <AppRouterCacheProvider>
      <ThemeModeProvider>
      <DemoEnvironmentContext.Provider value={demoEnv}>
        <main>
          <WSTAppBar />
          <NavDrawer />
          <div className="contentSection">{children}</div>
          <AlertBar/>
        </main>
        </DemoEnvironmentContext.Provider>
      </ThemeModeProvider>
    </AppRouterCacheProvider>
  );
}
