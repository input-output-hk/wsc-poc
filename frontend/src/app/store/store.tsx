//Zustand Imports
import { create } from "zustand";

//Local Imports
import { UserName, MenuTab } from "./types";

export type State = {
  mintAccount: string;
  currentUser: UserName;
  selectedTab: MenuTab;
  alertOpen: boolean;
  errorMessage: boolean;
};

export type Actions = {
  changeUserAccount: (newUser: UserName) => void;
  selectTab: (tab: MenuTab) => void;
  setAlertStatus: (status: boolean) => void;
};

const useStore = create<State & Actions>((set) => ({
    mintAccount: 'addr_test1qpju2uhn72ur6j5alln6nz7dqcgcjal7xjaw7lwdjdaex4qhr3xpz63fjwvlpsnu8efnhfdja78d3vkv8ks6ac09g3usemu2yl',
    currentUser: 'Mint Authority',
    selectedTab: 'Mint Actions',
    alertOpen: false,
    errorMessage: false,

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
