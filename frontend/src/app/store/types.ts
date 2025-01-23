export type UserName = 'Mint Authority' | 'User A' | 'User B' | 'Connected Wallet';
export type MenuTab = 'Mint Actions' | 'Addresses' | 'Wallet';
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
export type Severity = 'success' | 'error' | 'info' | 'warning';
export type AlertInfo = {
    open: boolean | undefined,
    severity: Severity,
    message: string,
    link?: string,
};