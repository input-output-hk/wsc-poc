import type { DemoEnvironment } from "../store/types";

export type DerivedAccountSnapshot = {
  regularAddress: string;
  programmableTokenAddress: string;
};

export type InitialWalletSnapshot = {
  mintAuthority: DerivedAccountSnapshot;
  alice: DerivedAccountSnapshot;
  bob: DerivedAccountSnapshot;
};

export type LoadedDemoEnvironment = {
  environment: DemoEnvironment;
  wasFallback: boolean;
};
