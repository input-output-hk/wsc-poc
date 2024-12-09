# Architecture

## Transactions

The proof-of-concept must facilitate the following *transaction families*:

1. Modify the access list. Replace the root of the merkle tree with a new hash.
2. Transfer ownership of regulated token from one address to another
3. Send regulated token back to token issuer (seizing funds)

Each of those transaction families will be implemented as a stake validator and "yields to" scripts for minting and spending policies.

Note that we don't consider the "setup transactions" here. For example, creating the initial access list, minting the access list NFT, first transfer of programmable token to programmable token validator. These are technically necessary to take the protocol from POC to product, but they are not required for the demonstration.

### 1. Modifying the access list

Transaction inputs
- UTxOs (spending)
  - Old access list UTxO
- UTxOs (reference)
  - Validator script 

Transaction outputs
- Access list UTxO
  - Datum: New access list (root of merkle tree)
  - Value: Access list NFT
  - Address: Validator that yields to stake script

Transaction logic
- Redeemer:
  * Min UTxO value
  * Index of input in list of inputs
- Conditions
  * Signature of token issuer must be present
  * No minting or burning of access list tokens
  * New access list outputs meets the requirements

### 2. Transfering ownership

Transaction inputs
- UTxOs (spending)
  - Old token locking validator

- UTxOs (reference)
  - Validator script
  - Access list UTxO with root of merkle tree

Transaction outputs
- Token locking UTxO
  - Datum: New owner
  - Value: 
  
Transaction logic
- Redeemer:
  * Min UTxO value
  * Index of input in list of inputs
  * Index of access list in list of reference inputs
  * Proofs of non-membership in merkle tree for current owner and new owner
- Conditions
  * Signature of current owner must be present
  * Both proofs of non-membership are valid
  * No minting or burning of programmable tokens

### 3. Seizing funds

Transaction inputs
- UTxOs (spending)
  - Old token locking validator
- UTxOs (reference)
  - Validator script
  - Access list UTxO

Transaction outputs
- Token locking UTxO
  - Datum: Owner = minting authority
  
Transaction logic
- Redeemer:
  * Min UTxO value
  * Index of input in list of inputs
  * Index of access list in list of reference inputs
  * Proof of membership of current owner in merkle tree
- Conditions
  * Signature of minting authority must be present
  * Proof of membership is valid
  * No minting or burning of programmable tokens

## API

A REST API must be implemented to faciliate the construction of transactions and to perform queries of ownership. It should have one endpoint for each of the three transactions listed above to construct a balanced transaction that can be signed by the user / minting authority.

The API does not need any kind of authentication as all the keys and related data are managed by the clients.