# System Design

This document describes the design of the regulated stablecoin proof-of-concept (POC).

## Overview

The POC consists of two components that make up the functionality of the regulated stablecoin.
The first component is CIP-XXX (TODO: Cip No. and link), which gives us a unified standard for managing different programmable tokens, comparable to some of the better-known ERC standards on Ethereum.
The second component is a concrete instance of such a programmable token, namely a policy that checks each transfer of tokens in its domain to ensure that the sender is not on a list of sanctioned addresses.
The policy also allows the issuer of the programmable token to seize funds from sanctioned addresses.

TODO: Diagram relationship CIP/policy

Both components are implemented in Plutarch. Besides the on-chain scripts, the POC also includes:

* Transaction building code for initial deployment, minting programmable tokens, transferring programmable tokens, adding addresses to the blacklist (ie. freezing), and seizing funds from blacklisted addresses
* Emulator tests for the nominal cases (happy path) based on the actual ledger implementation and mainnet protocol parameters
* A simple user interface that implements the use cases using browser-based wallets
* An OCI image with the on-chain code, the off-chain code and the UI

With the OCI image it is possible to run the complete system locally with just a single command.
There is no need to install the build toolchain or to operate a cardano node or related infrastructure.
The image can even be used to interact with existing deployments of the POC.

## High-Level Interactions

## On-Chain Scripts

* CIP related
* Policy related

### Performance

### Complexity

### Yielding Patterns

* Sequence diagrams explaining which script yields to what

## Off-Chain

### Docker Image, Deployment

### Blockfrost

### Lifecycle

* 2 phases: Deployment, operations

Each of the two components (programmable tokens, regulated stablecoin) requires an initial transaction that creates the on-chain data which is referenced on every interaction with the programmable tokens.
The initial transaction creates the registry nodes and mints the NFTs that are used to prove authenticity to the on-chain scripts.
In the POC, the initialisation procedures for programmable tokens and for the regulated stablecoin are contained in a single transaction.

### Security

The deployment phase relies exclusively on the command-line (CLI).
It builds a 
It does not use the web interface.
Therefre

## Limitations of POC