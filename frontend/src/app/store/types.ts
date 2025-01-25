export type UserName = 'Mint Authority' | 'Alice' | 'Bob' | 'Connected Wallet';
export type MenuTab = 'Mint Actions' | 'Addresses' | 'Wallet';
export type AccountInfo = {
    address: string,
    mnemonic: string,
    balance: number,
    adaBalance: number,
    status?: 'Active' | 'Frozen',
};
export type AccountKey = 'alice' | 'bob' | 'walletUser';
export type Accounts = {
    alice: AccountInfo;
    bob: AccountInfo;
    walletUser: AccountInfo;
};
export type Severity = 'success' | 'error' | 'info' | 'warning';
export type AlertInfo = {
    open: boolean | undefined,
    severity: Severity,
    message: string,
    link?: string,
};