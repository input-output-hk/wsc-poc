# Cardano eUTxO Vulnerability Scan Report

## Scan Metadata

- Scope: `ProgrammableLogicBase` / `ProgrammableLogicGlobal` path and directly-coupled off-chain builders.
- Commit: `aabd3d2f40ce881544561e2094af069f5853423f`
- Context artifact: `audit/context_report.md`
- Required artifacts present:
  - Entrypoint Table: present
  - State Graph: present
  - Branch Cards: present
  - Compliance Matrix: present
  - Threat Boundary Table: present

## Category Status

- Scanned: `DATUM_VALIDATION`
- Scanned: `ADDRESS_VALIDATION`
- Scanned: `DOUBLE_SATISFACTION`
- Scanned: `MINTING_POLICY`
- Scanned: `VALUE_VALIDATION`
- Scanned: `ACCESS_CONTROL`
- Scanned: `VALIDITY_RANGE`
- Scanned: `DENIAL_OF_SERVICE`
- Scanned: `ARITHMETIC`
- Scanned: `INPUT_VALIDATION`
- Scanned: `STATE_MANAGEMENT`
- Scanned: `STAKING_CREDENTIAL`
- Scanned: `LOGIC_ERROR`
- Scanned: `DOCUMENTATION`
- Scanned: `CONFIGURATION`
- Scanned: `ECONOMIC_DESIGN`
- Scanned: `UPGRADE_SAFETY`
- Scanned: `CODE_QUALITY`
- Scanned: `INFORMATION`
- N/A: `REENTRANCY` (EVM-only category)

## Summary Counts

- Critical: 0
- High: 0
- Medium: 0
- Low: 0
- Informational: 0
- Areas of Interest: 3

## Findings

No confirmed findings met mandatory finding criteria (concrete adversarial tx shape + code-path acceptance explanation + mitigation assessment).

## Areas of Interest

### AOI-1 [VALUE_VALIDATION] TransferAct minted-value interaction requires explicit proof tests

- Evidence:
  - `src/programmable-tokens-onchain/lib/SmartTokens/Contracts/ProgrammableLogicBase.hs:312`
  - `src/programmable-tokens-onchain/lib/SmartTokens/Contracts/ProgrammableLogicBase.hs:330`
  - `src/programmable-tokens-onchain/lib/SmartTokens/Contracts/Issuance.hs:129`
- Why AOI (not finding):
  - The code explicitly flags minted-value interaction as unfinished, but within scanned code we did not prove an accepting malicious transaction that violates current policy constraints.
  - Minting policy currently constrains mint destination behavior (`Issuance.hs:115`, `Issuance.hs:130`), which may mitigate some attack shapes.

### AOI-2 [LOGIC_ERROR/CONFIGURATION] Off-chain seize index semantics appear out of sync with on-chain contract

- Evidence:
  - On-chain relative-index traversal: `src/programmable-tokens-onchain/lib/SmartTokens/Contracts/ProgrammableLogicBase.hs:517`
  - WST off-chain uses absolute tx input indices: `src/examples/regulated-stablecoin/lib/Wst/Offchain/BuildTx/ProgrammableLogic.hs:119`
  - Redeemer field assignment from absolute index list: `src/examples/regulated-stablecoin/lib/Wst/Offchain/BuildTx/ProgrammableLogic.hs:135`
- Why AOI (not finding):
  - This indicates likely liveness/integration breakage, not an on-chain security acceptance path where malicious tx is accepted.

### AOI-3 [CODE_QUALITY/ARITHMETIC] Coercion-based delta containment path warrants targeted negative-delta tests

- Evidence:
  - Delta value built with coercion: `src/programmable-tokens-onchain/lib/SmartTokens/Contracts/ProgrammableLogicBase.hs:499`
  - Delta token subtraction can produce negatives: `src/programmable-tokens-onchain/lib/SmartTokens/Contracts/ProgrammableLogicBase.hs:686`, `src/programmable-tokens-onchain/lib/SmartTokens/Contracts/ProgrammableLogicBase.hs:701`
- Why AOI (not finding):
  - Without the exact in-repo definition of `pvalueContains` semantics and a reproducing adversarial tx acceptance path, this remains inconclusive.

## Categories with No Findings

- `DATUM_VALIDATION`
- `ADDRESS_VALIDATION`
- `DOUBLE_SATISFACTION`
- `MINTING_POLICY`
- `VALUE_VALIDATION`
- `ACCESS_CONTROL`
- `VALIDITY_RANGE`
- `DENIAL_OF_SERVICE`
- `ARITHMETIC`
- `INPUT_VALIDATION`
- `STATE_MANAGEMENT`
- `STAKING_CREDENTIAL`
- `LOGIC_ERROR`
- `DOCUMENTATION`
- `CONFIGURATION`
- `ECONOMIC_DESIGN`
- `UPGRADE_SAFETY`
- `CODE_QUALITY`
- `INFORMATION`
