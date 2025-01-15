export type UserName = 'Mint Authority' | 'User A' | 'User B' | 'Connected Wallet';
export type MenuTab = 'Mint Actions' | 'Accounts' | 'Wallet';
export type AccountInfo = {
    address: string,
    mnemonic: string,
    balance: number,
    status?: 'Active' | 'Frozen',
};
export type AccountKey = 'userA' | 'userB' | 'walletUser';
export type Accounts = {
    userA: AccountInfo;
    userB: AccountInfo;
    walletUser: AccountInfo;
};