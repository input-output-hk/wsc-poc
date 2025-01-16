//Zustand Imports
import { create } from "zustand";

//Axios imports
import axios from 'axios';

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
  fetchBlacklistStatus: (accounts: Accounts) => void;
};

const useStore = create<State & Actions>((set) => ({
    mintAccount: {
      name: 'Mint Authority',
      address: 'addr_test1qq986m3uel86pl674mkzneqtycyg7csrdgdxj6uf7v7kd857kquweuh5kmrj28zs8czrwkl692jm67vna2rf7xtafhpqk3hecm',
      mnemonic: 'problem alert infant glance toss gospel tonight sheriff match else hover upset chicken desert anxiety cliff moment song large seed purpose chalk loan onion',
      balance: 0,
    },
    accounts: {
      userA: {
        address: '',
        mnemonic: 'during dolphin crop lend pizza guilt hen earn easy direct inhale deputy detect season army inject exhaust apple hard front bubble emotion short portion',
        balance: 0,
        status: 'Active',
      },
      userB: {
        address: '',
        mnemonic: 'tooth benefit wish capable stock inner motor cover diamond crash work amount foot help shell glad friend front degree pudding inflict filter twice resource',
        balance: 0,
        status: 'Active',
      },
      walletUser: {
        address: '',
        mnemonic: '',
        balance: 0,
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

    fetchBlacklistStatus: async (accounts: Accounts) => {
      try {
        // const requests = Object.entries(accounts).map(async ([key, account]) => {
        //   const response = await axios.get(`/api/v1/query/blacklist/${account.address}`, {
        //     headers: {
        //       'Content-Type': 'application/json;charset=utf-8',
        //       Accept: 'application/json;charset=utf-8',
        //     },
        //   });
        //   return { key, data: response.data };
        // });
    
        // console.log(requests);

        const response = await axios.get(`/api/v1/query/blacklist/${accounts.userA.address}`, {
          headers: {
            'Content-Type': 'application/json;charset=utf-8',
            Accept: 'application/json;charset=utf-8',
          },
        });
        return { 'blacklist response userA': response };
      
        // const results = await Promise.all(requests);
    
        // return results;
      } catch (error) {
        console.error('Blacklist fetch failed:', error);
        throw new Error('Blacklist fetch failed');
      }
    },

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
          case 'User A':
          case 'User B':
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
