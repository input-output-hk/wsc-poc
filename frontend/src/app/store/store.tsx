//Zustand Imports
import { create } from "zustand";

//Local Imports
import { UserName, AccountInfo, MenuTab } from "./types";

export type State = {
  mintAccount: AccountInfo;
  userA: AccountInfo;
  userB: AccountInfo;
  currentUser: UserName;
  selectedTab: MenuTab;
  alertOpen: boolean;
  errorMessage: boolean;
};

export type Actions = {
  changeMintAccountDetails: (newAccountInfo: AccountInfo) => void;
  changeWalletAAccountDetails: (newAccountInfo: AccountInfo) => void;
  changeWalletBAccountDetails: (newAccountInfo: AccountInfo) => void;
  changeUserAccount: (newUser: UserName) => void;
  selectTab: (tab: MenuTab) => void;
  setAlertStatus: (status: boolean) => void;
};

const useStore = create<State & Actions>((set) => ({
    mintAccount: {
      keyAgent: undefined,
      address: 'addr_test1qq986m3uel86pl674mkzneqtycyg7csrdgdxj6uf7v7kd857kquweuh5kmrj28zs8czrwkl692jm67vna2rf7xtafhpqk3hecm',
      mnemonic: ['problem', 'alert', 'infant', 'glance', 'toss', 'gospel', 'tonight', 'sheriff', 'match', 'else', 'hover', 'upset', 'chicken', 'desert', 'anxiety', 'cliff', 'moment', 'song', 'large', 'seed', 'purpose', 'chalk', 'loan', 'onion'],
      balance: 5
    },
    userA: {
      keyAgent: undefined,
      address: '',
      mnemonic: [],
      balance: 0,
      status: 'Active',
    },
    userB: {
      keyAgent: undefined,
      address: '',
      mnemonic: [],
      balance: 0,
      status: 'Active',
    },
    currentUser: 'Mint Authority',
    selectedTab: 'Mint Actions',
    alertOpen: false,
    errorMessage: false,

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
}));

export default useStore;
