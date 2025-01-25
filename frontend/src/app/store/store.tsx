//Zustand Imports
import { create } from "zustand";

//Local Imports
import { UserName, AccountInfo, Accounts, MenuTab, AccountKey, AlertInfo } from "./types";
import { LucidEvolution } from "@lucid-evolution/lucid";

export type State = {
  mintAccount: AccountInfo;
  accounts: Accounts;
  blacklistAddresses: string[];
  currentUser: UserName;
  selectedTab: MenuTab;
  alertInfo: AlertInfo; 
  lucid: LucidEvolution;
};

export type Actions = {
  changeMintAccountDetails: (newAccountInfo: AccountInfo) => void;
  changeWalletAccountDetails: (accountKey: AccountKey, newAccountInfo: AccountInfo) => void;
  changeUserAccount: (newUser: UserName) => void;
  selectTab: (tab: MenuTab) => void;
  changeAlertInfo: (alertInfo: AlertInfo) => void;
  setLucidInstance: (lucid: LucidEvolution) => void;
};

const useStore = create<State & Actions>((set) => ({
    mintAccount: {
      name: 'Mint Authority',
      address: 'addr_test1qq986m3uel86pl674mkzneqtycyg7csrdgdxj6uf7v7kd857kquweuh5kmrj28zs8czrwkl692jm67vna2rf7xtafhpqk3hecm',
      mnemonic: 'problem alert infant glance toss gospel tonight sheriff match else hover upset chicken desert anxiety cliff moment song large seed purpose chalk loan onion',
      balance: {ada: 0, wst: 0},
    },
    accounts: {
      alice: {
        address: '',
        mnemonic: 'during dolphin crop lend pizza guilt hen earn easy direct inhale deputy detect season army inject exhaust apple hard front bubble emotion short portion',
        balance: {ada: 0, wst: 0},
        status: 'Active',
      },
      bob: {
        address: '',
        mnemonic: 'silver legal flame powder fence kiss stable margin refuse hold unknown valid wolf kangaroo zero able waste jewel find salad sadness exhibit hello tape',
        balance: {ada: 0, wst: 0},
        status: 'Active',
      },
      walletUser: {
        address: '',
        mnemonic: '',
        balance: {ada: 0, wst: 0},
        status: 'Active',
      },
    },
    blacklistAddresses: [],
    currentUser: 'Mint Authority',
    selectedTab: 'Mint Actions',
    alertInfo: {
      open: false,
      message: 'Transaction sent successfully!',
      severity: 'success',
    },
    lucid: {} as LucidEvolution,

    changeMintAccountDetails: (newAccountInfo: AccountInfo) => {
      set(() => {
        return { mintAccount: newAccountInfo };
      });
    },

    changeWalletAccountDetails: (accountKey, newAccountInfo) => {
      set((state) => ({
        accounts: {
          ...state.accounts,
          [accountKey]: newAccountInfo,
        },
      }));
    },

    changeUserAccount: (newUser: UserName) => {
        let firstAccessibleTab: MenuTab;
        switch (newUser) {
          case 'Mint Authority':
            firstAccessibleTab = 'Mint Actions'; 
            break;
          case 'Alice':
          case 'Bob':
            firstAccessibleTab = 'Wallet'; 
            break;
          case 'Connected Wallet':
            if (useStore.getState().accounts.walletUser.address === useStore.getState().mintAccount.address)
              firstAccessibleTab = 'Mint Actions';
            else
              firstAccessibleTab = 'Wallet';
            break
          default:
            firstAccessibleTab = 'Mint Actions';
        }
        set({ currentUser: newUser, selectedTab: firstAccessibleTab });
    },

    selectTab: (tab: MenuTab) => {
        set({ selectedTab: tab });
    },

    changeAlertInfo: (partial: Partial<AlertInfo>) => {
      set((state) => ({
        alertInfo: {
          ...state.alertInfo,
          ...partial,
        },
      }));
    },

    setLucidInstance: (lucid) => {
      set({ lucid: lucid });
    }
}));  

export default useStore;