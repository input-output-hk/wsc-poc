# Regulated stablecoin POC

This is a proof-of-concept for a regulated stablecoin. It is NOT a finished product.

# Overview

The POC is based on [CIP-0143](https://github.com/colll78/CIPs/blob/patch-3/CIP-0143/README.md), instantiated with a programmable logic that checks whether the target address is blacklisted before allowing a transfer of the programmable token from one owner to another.

# Architecture

The system is designed so that all actions except the initial deployment of the programmable logic UTxOs can be performed through a web UI with browser-based wallets. The REST API therefore exposes a number of endpoints that produce fully-balanced-but-not-signed transactions. The intention is for the caller (web UI) to sign the transactions with the web-based wallet and submit them to the network. The backend uses blockfrost to query the blockchain. As a result, the server is pretty light-weight and does not even need its own database or a full cardano node.

# Usage

There is a CLI tool `wst-poc-cli` that performs the initial deployment of the system and runs the REST server. A signing key file is needed for the initial deployment but not for the operation of the server. A blockfrost token is needed for both the initial deployment.

(TO DO - document CLI operations)

# FAQs

## How is this system different from Djed?

Djed is an algorithmic stablecoin that is backed by Ada. In Djed we keep the entire reserves of the stablecoin in a UTxO that is controlled by the Djed contract. Every user of Djed can verify that the reserves exist and that there is enough Ada to pay out all Djed holders.

This POC implements a _fiat-backed stablecoin_. This means that the reserves exist in a bank account outside of the blockchain, and we have to trust the issuer of the stablecoin that every token that's been issued on-chain is backed by one USD in the bank account.

From a technical perspective, not having to manage the reserve on-chain makes the design of this POC somewhat simpler: We don't need to maintain a global state (the Djed UTxO) that all orders have to synchronise with. The challenge in this POC lies in the programmable token logic.

## How does the system scale?

The core idea of the regulated stablecoin is to run a check every time that the owner of some amount of regulated tokens changes. This check is performed by the _transfer logic script_, a plutus program that consults a list of sanctioned addresses to ensure that the receiving address is not on it. 

The list of sanctioned addresses is the only data structure that (a) needs to be read from by every transaction of the transfer logic script and (b) gets changed regularly during the operation of the stablecoin.

All other factors (number of scripts, script budget, max. number of transfer checks per transaction and so forth) are fixed and do not depend on the number of users.

It is important to note that the list of sanctioned addresses scaled in space (number of UTxOs), but working with the data structure is done in constant time due to the way the data is laid out.

There is also no risk of UTxO congestion as the "system outputs" are used as reference inputs and not spent by user-to-user transfers.

### Sanctioned Addresses

The list of sanctioned addresses is stored on-chain as a _linked list_. This means that each entry (address) in the list is represented single transaction output that includes the address itself as well as a pointer to the next address in lexicographical order.

When checking a transfer the transfer logic script is provided with a single reference input pointing to the relevant entry in the ordered linked list. 

The transfer transaction does not spend the linked list output, therefore the same linked list output can be used by many transactions in the same block and across multiple blocks.

#### How many sanctioned addresses are there?

Publicly available data on Tether (the largest fiat stablecoin) [1] [2] indicates that Tether has a total of [1990 sanctioned addresses](https://dune.com/phabc/usdt---banned-addresses), out of [109 million on-chain wallets](https://tether.io/news/how-many-usdt-on-chain-holders-are-there/) (Dec. 2024). This suggests that about 0.002 percent of addresses need to be blacklisted.

If our system achieved the scale of Tether then we would need about 1200 UTxOs to store the linked list. At current Ada prices this would amount to 1800 USD in min Ada UTxO deposits, an amount that will be refunded in its entirety when the linked list is deleted.

USDC, another fiat-stablecoin, currently has [264 blacklisted addresses](https://bloxy.info/txs/events_sc/0xa0b86991c6218b36c1d19d4a2e9eb0ce3606eb48?signature_id=257159) and 3m users, with a blacklist ratio of about 0.009%.

### 

# Contributing

Run the tests with `cabal test all`.

Bug reports and contributions are welcome!
