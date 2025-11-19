import "server-only";

import { walletFromSeed } from "@lucid-evolution/lucid";

import type { DerivedAccountSnapshot, InitialWalletSnapshot, LoadedDemoEnvironment } from "./initialData";
import { DemoEnvironment, previewEnv } from "../store/types";

let cachedDemoEnvironment: DemoEnvironment | null = null;

const resolveBaseUrl = () => {
  const raw =
    process.env.NEXT_PUBLIC_API_URL ?? // Production API URL
    process.env.API_BASE_URL ??
    process.env.NEXT_PUBLIC_API_BASE_URL ??
    process.env.NEXT_PUBLIC_BACKEND_BASE_URL ??
    process.env.NEXT_PUBLIC_SITE_URL ??
    process.env.SITE_URL ??
    (process.env.VERCEL_URL ? `https://${process.env.VERCEL_URL}` : undefined);

  if (raw && /^https?:\/\//i.test(raw)) {
    return raw;
  }
  if (raw && !/^https?:\/\//i.test(raw)) {
    return `https://${raw}`;
  }
  const port = process.env.PORT ?? '3000';
  return `http://localhost:${port}`;
};

const BASE_URL = resolveBaseUrl();

const buildUrl = (path: string) => {
  try {
    return new URL(path, BASE_URL).toString();
  } catch (error) {
    console.warn(`Unable to resolve URL for ${path}, falling back to relative path.`, error);
    return path;
  }
};

async function fetchJson<T>(path: string): Promise<T> {
  const response = await fetch(buildUrl(path), {
    cache: "no-store",
  });
  if (!response.ok) {
    throw new Error(`Failed to fetch ${path}: ${response.status} ${response.statusText}`);
  }
  return response.json() as Promise<T>;
}

export async function loadDemoEnvironment(): Promise<LoadedDemoEnvironment> {
  try {
    const environment = await fetchJson<DemoEnvironment>("/api/v1/demo-environment");
    cachedDemoEnvironment = environment;
    return { environment, wasFallback: false };
  } catch (error) {
    console.warn("Falling back to cached demo environment", error);
    if (cachedDemoEnvironment) {
      return { environment: cachedDemoEnvironment, wasFallback: true };
    }
    return { environment: previewEnv, wasFallback: true };
  }
}

async function fetchProgrammableTokenAddress(address: string): Promise<string> {
  try {
    return await fetchJson<string>(`/api/v1/query/address/${address}`);
  } catch (error) {
    console.warn("Failed to fetch programmable token address", address, error);
    return "";
  }
}

async function deriveWalletSnapshot(env: DemoEnvironment, mnemonic: string): Promise<DerivedAccountSnapshot> {
  const wallet = await walletFromSeed(mnemonic, {
    password: "",
    addressType: "Base",
    accountIndex: 0,
    network: env.network,
  });
  const programmableTokenAddress = await fetchProgrammableTokenAddress(wallet.address);
  return {
    regularAddress: wallet.address,
    programmableTokenAddress,
  };
}

export async function buildInitialWalletSnapshot(env: DemoEnvironment): Promise<InitialWalletSnapshot> {
  const [mintAuthority, alice, bob] = await Promise.all([
    deriveWalletSnapshot(env, env.mint_authority),
    deriveWalletSnapshot(env, env.user_a),
    deriveWalletSnapshot(env, env.user_b),
  ]);

  return {
    mintAuthority,
    alice,
    bob,
  };
}
