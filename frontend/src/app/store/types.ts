import { InMemoryKeyAgent } from '@cardano-sdk/key-management';

export type UserName = 'Mint Authority' | 'User A' | 'User B' | 'Connected Wallet';
export type MenuTab = 'Mint Actions' | 'Accounts' | 'Wallet';
export type AccountInfo = {
    keyAgent: InMemoryKeyAgent | undefined,
    address: string,
    mnemonic: string[],
    balance: number,
    status?: 'Active' | 'Frozen',
}