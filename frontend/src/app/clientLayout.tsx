"use client";
//React imports 
import React, { useEffect, useState } from "react";

//Mui imports
import { AppRouterCacheProvider } from '@mui/material-nextjs/v15-appRouter';
import NavDrawer from './components/NavDrawer';

//Local file
import { ThemeModeProvider } from "./styles/themeContext";
import "./styles/globals.css";
import { makeLucid, getWalletFromSeed } from "./utils/walletUtils";
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
            const mintAuthorityWallet = await getWalletFromSeed(demoEnv.mint_authority);
            const walletA = await getWalletFromSeed(demoEnv.user_a);
            const walletB = await getWalletFromSeed(demoEnv.user_b);
    
            // Update Zustand store with the initialized wallet information
            changeMintAccountDetails({ ...mintAccount, address: mintAuthorityWallet.address});
            changeWalletAccountDetails('alice', { ...accounts.alice, address: walletA.address},);
            changeWalletAccountDetails('bob', { ...accounts.bob, address: walletB.address});
    
            const initialLucid = await makeLucid(demoEnv);
            setLucidInstance(initialLucid);
            console.log('Wallets initialized');
          } catch (error) {
            console.error('Error initializing wallets:', error);
          }
        };
    
        fetchUserWallets();
      },[]);

  if(accounts.bob.address === '') {
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
