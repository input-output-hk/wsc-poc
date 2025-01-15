//Zustand Imports
import { create } from "zustand";

//Local Imports
import { UserName, AccountInfo, MenuTab } from "./types";
import { LucidEvolution } from "@lucid-evolution/lucid";

export type State = {
  mintAccount: AccountInfo;
  userA: AccountInfo;
  userB: AccountInfo;
  walletUser: AccountInfo;
  currentUser: UserName;
  selectedTab: MenuTab;
  alertOpen: boolean;
  errorMessage: boolean;
  lucid: LucidEvolution;
};

export type Actions = {
  changeMintAccountDetails: (newAccountInfo: AccountInfo) => void;
  changeWalletAAccountDetails: (newAccountInfo: AccountInfo) => void;
  changeWalletBAccountDetails: (newAccountInfo: AccountInfo) => void;
  changeToLaceWallet: (newAccountInfo: AccountInfo) => void;
  changeUserAccount: (newUser: UserName) => void;
  selectTab: (tab: MenuTab) => void;
  setAlertStatus: (status: boolean) => void;
  setLucidInstance: (lucid: LucidEvolution) => void;
};

const useStore = create<State & Actions>((set) => ({
    mintAccount: {
      address: 'addr_test1qq986m3uel86pl674mkzneqtycyg7csrdgdxj6uf7v7kd857kquweuh5kmrj28zs8czrwkl692jm67vna2rf7xtafhpqk3hecm',
      mnemonic: 'problem alert infant glance toss gospel tonight sheriff match else hover upset chicken desert anxiety cliff moment song large seed purpose chalk loan onion',
      balance: 0,
    },
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
    walletUser:
    {
      address: '',
      mnemonic: '',
      balance: 0,
      status: 'Active',
    },
    currentUser: 'Mint Authority',
    selectedTab: 'Mint Actions',
    alertOpen: false,
    errorMessage: false,
    lucid: {} as LucidEvolution,

    changeMintAccountDetails: (newAccountInfo: AccountInfo) => {
      set(() => {
        return { mintAccount: newAccountInfo };
      });
    },

    changeWalletAAccountDetails: (newAccountInfo: AccountInfo) => {
      set(() => {
        return { userA: newAccountInfo };
      });
    },
    
    changeWalletBAccountDetails: (newAccountInfo: AccountInfo) => {
      set(() => {
        return { userB: newAccountInfo };
      });
    },

    changeToLaceWallet: (newAccountInfo: AccountInfo) => {
      set(() => {
       return { walletUser: newAccountInfo }
      });
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
            if (useStore.getState().walletUser.address === useStore.getState().mintAccount.address)
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

    setAlertStatus: (status: boolean) => {
        set({ alertOpen: status });
    },

    setLucidInstance: (lucid) => {
      set({ lucid: lucid });
    }
}));  

export default useStore;
