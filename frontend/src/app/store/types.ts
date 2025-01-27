export type UserName = 'Mint Authority' | 'Alice' | 'Bob' | 'Connected Wallet';
export type MenuTab = 'Mint Actions' | 'Addresses' | 'Wallet';
import { Network } from "@lucid-evolution/lucid";

export type AccountInfo = {
    address: string,
    balance: WalletBalance,
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
export type WalletBalance = { wst: number, ada: number }

// This should correspond to
// Wst.Server.DemoEnvironment.DemoEnvironment
export type DemoEnvironment = {
  mint_authority: string,
  user_a: string,
  user_b: string,
  blockfrost_url: string,
  blockfrost_key: string,
  minting_policy: string,
  token_name: string,
  transfer_logic_address: string,
  prog_logic_base_hash: string,
  network: Network,
  explorer_url: string
}

const previewEnv: DemoEnvironment = {
  mint_authority: "problem alert infant glance toss gospel tonight sheriff match else hover upset chicken desert anxiety cliff moment song large seed purpose chalk loan onion",
  user_a: "during dolphin crop lend pizza guilt hen earn easy direct inhale deputy detect season army inject exhaust apple hard front bubble emotion short portion",
  user_b: "silver legal flame powder fence kiss stable margin refuse hold unknown valid wolf kangaroo zero able waste jewel find salad sadness exhibit hello tape",
  blockfrost_url: "https://cardano-preview.blockfrost.io/api/v0",
  explorer_url: "https://preview.cexplorer.io/tx",
  blockfrost_key: "",
  minting_policy: "b34a184f1f2871aa4d33544caecefef5242025f45c3fa5213d7662a9",
  token_name: "575354",
  transfer_logic_address: "addr_test1qq986m3uel86pl674mkzneqtycyg7csrdgdxj6uf7v7kd857kquweuh5kmrj28zs8czrwkl692jm67vna2rf7xtafhpqk3hecm",
  prog_logic_base_hash: "fca77bcce1e5e73c97a0bfa8c90f7cd2faff6fd6ed5b6fec1c04eefa",
  network: "Preview"
}

export { previewEnv };
