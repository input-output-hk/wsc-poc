'use client';

// Zustand Imports
import { createWithEqualityFn } from 'zustand/traditional';
import { persist } from 'zustand/middleware';
import { shallow } from 'zustand/shallow';
import type { StateCreator } from 'zustand';

// Local Imports
import { UserName, AccountInfo, Accounts, MenuTab, AccountKey, AlertInfo } from "./types";
import { LucidEvolution } from "@lucid-evolution/lucid";

type AccountsSlice = {
  mintAccount: AccountInfo;
  accounts: Accounts;
  blacklistAddresses: string[];
  changeMintAccountDetails: (newAccountInfo: AccountInfo) => void;
  changeWalletAccountDetails: (accountKey: AccountKey, newAccountInfo: AccountInfo) => void;
};

type UiSlice = {
  currentUser: UserName;
  selectedTab: MenuTab | null;
  alertInfo: AlertInfo;
  hasHydrated: boolean;
  changeUserAccount: (newUser: UserName) => void;
  selectTab: (tab: MenuTab) => void;
  changeAlertInfo: (partial: Partial<AlertInfo>) => void;
  setHasHydrated: (value: boolean) => void;
};

type LucidSlice = {
  lucid: LucidEvolution;
  setLucidInstance: (lucid: LucidEvolution) => void;
};

export type StoreState = AccountsSlice & UiSlice & LucidSlice;

const emptyAccount = (): AccountInfo => ({
  regular_address: '',
  programmable_token_address: '',
  balance: { ada: 0, wst: 0, adaOnlyOutputs: 0 },
  status: 'Active',
  hasRegisteredScripts: false,
});

const initialAccounts = (): Accounts => ({
  alice: emptyAccount(),
  bob: emptyAccount(),
  walletUser: emptyAccount(),
});

const initialAlertInfo = (): AlertInfo => ({
  id: 0,
  open: false,
  message: 'Transaction sent successfully!',
  severity: 'success',
});

const firstAccessibleTab = (user: UserName, walletUserAddress: string, mintAddress: string): MenuTab | null => {
  switch (user) {
    case 'Mint Authority':
      return 'Mint Actions';
    case 'Alice':
    case 'Bob':
      return 'Wallet';
    case 'Connected Wallet':
      return walletUserAddress !== '' && walletUserAddress === mintAddress ? 'Mint Actions' : 'Wallet';
    default:
      return null;
  }
};

const createAccountsSlice: StateCreator<
  StoreState,
  [],
  [],
  AccountsSlice
> = (set, _get, _api) => {
  void _get;
  void _api;
  return {
    mintAccount: {
      name: 'Mint Authority',
      regular_address: '',
      programmable_token_address: '',
      balance: { ada: 0, wst: 0, adaOnlyOutputs: 0 },
    },
    accounts: initialAccounts(),
    blacklistAddresses: [],

    changeMintAccountDetails: (newAccountInfo) => {
      set(() => ({
        mintAccount: newAccountInfo,
      }));
    },

    changeWalletAccountDetails: (accountKey, newAccountInfo) => {
      set((state) => ({
        accounts: {
          ...state.accounts,
          [accountKey]: newAccountInfo,
        },
      }));
    },
  };
};

const createUiSlice: StateCreator<
  StoreState,
  [],
  [],
  UiSlice
> = (set, get, _api) => {
  void _api;
  return {
  currentUser: 'Not Connected',
  selectedTab: null,
  alertInfo: initialAlertInfo(),
  hasHydrated: false,

  changeUserAccount: (newUser) => {
    const { accounts, mintAccount } = get();
    const tab = firstAccessibleTab(newUser, accounts.walletUser.regular_address, mintAccount.regular_address);
    set({ currentUser: newUser, selectedTab: tab });
  },

  selectTab: (tab) => {
    set({ selectedTab: tab });
  },

  changeAlertInfo: (partial) => {
    set((state) => {
      const next: AlertInfo = {
        ...state.alertInfo,
        ...partial,
      };
      if (partial.open) {
        next.id = Date.now();
        next.open = true;
      }
      if (partial.open === false) {
        next.open = false;
      }
      return { alertInfo: next };
    });
  },

  setHasHydrated: (value) => set({ hasHydrated: value }),
  };
};

const createLucidSlice: StateCreator<
  StoreState,
  [],
  [],
  LucidSlice
> = (set, _get, _api) => {
  void _get;
  void _api;
  return {
  lucid: {} as LucidEvolution,
  setLucidInstance: (lucid) => set({ lucid }),
  };
};

const createStore = (set: any, get: any, api: any) => ({
  ...createAccountsSlice(set, get, api),
  ...createUiSlice(set, get, api),
  ...createLucidSlice(set, get, api),
});

const mergePersistedState = (persisted: any, currentState: StoreState): StoreState => {
  if (!persisted) {
    return currentState;
  }
  const persistedAccounts = persisted.accounts ?? {};
  return {
    ...currentState,
    ...persisted,
    accounts: {
      ...currentState.accounts,
      ...persistedAccounts,
      walletUser: {
        ...currentState.accounts.walletUser,
        ...persistedAccounts.walletUser,
      },
    },
  };
};

const useStoreBase = createWithEqualityFn<StoreState>()(
  persist(createStore, {
    name: 'wst-store',
    version: 1,
    partialize: (state) => ({
      currentUser: state.currentUser,
      accounts: {
        walletUser: state.accounts.walletUser,
      },
      selectedTab: state.selectedTab,
    }),
    merge: (persistedState, currentState) => mergePersistedState(persistedState, currentState),
    onRehydrateStorage: () => (state) => {
      if (!state) {
        return;
      }
      const walletConnected = Boolean(state.accounts.walletUser.regular_address);
      if (!walletConnected) {
        state.changeUserAccount('Not Connected');
      }
      state.setHasHydrated(true);
    },
  }),
  shallow
);

const useStore = useStoreBase;

export default useStore;
