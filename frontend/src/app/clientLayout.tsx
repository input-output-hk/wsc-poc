"use client";
//React imports 
import React, { useEffect, useMemo, useState } from "react";

//Mui imports
import { AppRouterCacheProvider } from '@mui/material-nextjs/v15-appRouter';
import Box from '@mui/material/Box';
import Skeleton from '@mui/material/Skeleton';
import Typography from '@mui/material/Typography';
import NavDrawer from './components/NavDrawer';

//Local files
import { ThemeModeProvider } from "./styles/themeContext";
import "./styles/globals.css";
import { makeLucid } from "./utils/walletUtils";
import useStore from './store/store'; 
import WSTAppBar from "./components/WSTAppBar";
import AlertBar from './components/AlertBar';
import { DemoEnvironment } from "./store/types";
import DemoEnvironmentContext from "./context/demoEnvironmentContext";
import type { InitialWalletSnapshot } from "./lib/initialData";
import WSTCommonButton from "./components/WSTCommonButton";
import { shallow } from 'zustand/shallow';

type ClientLayoutProps = {
  children: React.ReactNode;
  initialDemoEnvironment: DemoEnvironment;
  initialWallets: InitialWalletSnapshot;
};

type InitStatus = "loading" | "ready" | "error";

const AppLoadingSkeleton = ({
  status,
  errorMessage,
  onRetry,
}: {
  status: InitStatus;
  errorMessage?: string | null;
  onRetry?: () => void;
}) => (
  <Box sx={{ height: '100vh', width: '100vw', display: 'flex', flexDirection: 'column', gap: 2, p: 2 }}>
    <Skeleton variant="rectangular" height={56} sx={{ borderRadius: 2 }} />
    <Box sx={{ display: 'flex', flex: 1, gap: 2, overflow: 'hidden' }}>
      <Skeleton variant="rectangular" width={220} sx={{ borderRadius: 2 }} />
      <Box sx={{ flex: 1, display: 'grid', gridTemplateColumns: 'repeat(auto-fit, minmax(280px, 1fr))', gap: 2 }}>
        {Array.from({ length: 4 }).map((_, index) => (
          <Skeleton key={index} variant="rectangular" height={220} sx={{ borderRadius: 2 }} />
        ))}
        <Skeleton variant="rectangular" height={350} sx={{ borderRadius: 2 }} />
      </Box>
    </Box>
    {status === 'error' && (
      <Box sx={{ textAlign: 'center' }}>
        <Typography variant="body1" color="error" sx={{ mb: 1 }}>
          {errorMessage ?? 'Unable to prepare wallet state.'}
        </Typography>
        {onRetry && <WSTCommonButton text="Retry initialisation" onClick={onRetry} size="small" />}
      </Box>
    )}
  </Box>
);

export default function ClientLayout({ children, initialDemoEnvironment, initialWallets }: ClientLayoutProps) {
  const { changeMintAccountDetails, changeWalletAccountDetails, setLucidInstance } = useStore(
    (state) => ({
      changeMintAccountDetails: state.changeMintAccountDetails,
      changeWalletAccountDetails: state.changeWalletAccountDetails,
      setLucidInstance: state.setLucidInstance,
    }),
    shallow
  );
  const alertInfo = useStore((state) => state.alertInfo);
  const changeAlertInfo = useStore((state) => state.changeAlertInfo);
  const [initStatus, setInitStatus] = useState<InitStatus>("loading");
  const [initError, setInitError] = useState<string | null>(null);
  const [initAttempt, setInitAttempt] = useState(0);

  useEffect(() => {
    let cancelled = false;
    const initialise = async () => {
      setInitStatus("loading");
      setInitError(null);
      try {
        const { mintAccount: currentMintAccount, accounts: currentAccounts } = useStore.getState();
        changeMintAccountDetails({
          ...currentMintAccount,
          regular_address: initialWallets.mintAuthority.regularAddress,
          programmable_token_address: initialWallets.mintAuthority.programmableTokenAddress,
        });
        changeWalletAccountDetails("alice", {
          ...currentAccounts.alice,
          regular_address: initialWallets.alice.regularAddress,
          programmable_token_address: initialWallets.alice.programmableTokenAddress,
        });
        changeWalletAccountDetails("bob", {
          ...currentAccounts.bob,
          regular_address: initialWallets.bob.regularAddress,
          programmable_token_address: initialWallets.bob.programmableTokenAddress,
        });

        const lucid = await makeLucid(initialDemoEnvironment);
        if (cancelled) {
          return;
        }
        setLucidInstance(lucid);
        setInitStatus("ready");
      } catch (error) {
        console.error("Error initializing wallets:", error);
        if (!cancelled) {
          setInitError(error instanceof Error ? error.message : "Unknown error");
          setInitStatus("error");
        }
      }
    };

    initialise();
    return () => {
      cancelled = true;
    };
  }, [
    changeMintAccountDetails,
    changeWalletAccountDetails,
    initialDemoEnvironment,
    initialWallets,
    initAttempt,
    setLucidInstance,
  ]);

  const handleRetry = () => setInitAttempt((prev) => prev + 1);
  const demoEnv = useMemo(() => initialDemoEnvironment, [initialDemoEnvironment]);

  return (
    <AppRouterCacheProvider>
      <ThemeModeProvider>
        <DemoEnvironmentContext.Provider value={demoEnv}>
          {initStatus !== "ready" ? (
            <AppLoadingSkeleton status={initStatus} errorMessage={initError} onRetry={initStatus === "error" ? handleRetry : undefined} />
          ) : (
            <main>
              <WSTAppBar />
              <NavDrawer />
              <div className="contentSection">{children}</div>
              <AlertBar alertInfo={alertInfo} onClose={() => changeAlertInfo({ open: false })} />
            </main>
          )}
        </DemoEnvironmentContext.Provider>
      </ThemeModeProvider>
    </AppRouterCacheProvider>
  );
}
