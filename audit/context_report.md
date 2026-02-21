# Cardano eUTxO Context Report

## Scope Manifest

- Target repository root: `/home/gumbo/iohk/wsc-poc`
- Target component: `ProgrammableLogicBase` (including its delegated global stake validator path)
- Commit: `aabd3d2f40ce881544561e2094af069f5853423f`
- Included modules (scope):
  - `src/programmable-tokens-onchain/lib/SmartTokens/Contracts/ProgrammableLogicBase.hs`
  - `src/programmable-tokens-onchain/lib/SmartTokens/Contracts/Issuance.hs`
  - `src/programmable-tokens-offchain/lib/ProgrammableTokens/OffChain/Scripts.hs`
  - `src/programmable-tokens-offchain/lib/ProgrammableTokens/OffChain/BuildTx/ProgrammableLogic.hs`
  - `src/examples/regulated-stablecoin/lib/Wst/Offchain/BuildTx/ProgrammableLogic.hs`
  - `src/programmable-tokens-test/test/ProgrammableTokens/Test/ProgrammableLogicGlobal.hs`
  - `src/programmable-tokens-test/exe/BenchmarkOnchainScripts.hs`
  - `doc/architecture.md`
- Excluded modules:
  - Directory linked-list mint/spend internals not directly on the programmable logic base/global path.
  - Example ACP blacklist internals except where they affect programmable logic invocation assumptions.
- Spec/document sources used:
  - `doc/architecture.md:56`
  - `doc/architecture.md:61`
  - `doc/architecture.md:62`
  - `README.md:3`

Unresolved scope questions:
- Exact semantic contract of `pvalueContains` under negative token quantities is external to this repository and was treated as an assumption boundary in this scoped review.

## Entrypoint Table

| Entrypoint | Script purpose | Branch/redeemer | Required signers / witnesses | Required references | Control assets / anchors | Continuing output constraints | Value movement rule |
|---|---|---|---|---|---|---|---|
| `mkProgrammableLogicBase` (`ProgrammableLogicBase.hs:179`) | Spending | single path | Requires withdrawal entry containing the configured global stake credential (`ProgrammableLogicBase.hs:187`, `ProgrammableLogicBase.hs:196`) | none | none directly | none directly | Base validator delegates logic to global script; no independent value checks (`ProgrammableLogicBase.hs:176`, `ProgrammableLogicBase.hs:196`) |
| `mkProgrammableLogicGlobal` (`ProgrammableLogicBase.hs:284`) | Rewarding | `TransferAct` (`ProgrammableLogicBase.hs:311`) | For each spent base input: stake-owner witness required (pk signature or script withdrawal) via `pvalueFromCred` (`ProgrammableLogicBase.hs:131`, `ProgrammableLogicBase.hs:137`, `ProgrammableLogicBase.hs:142`) | Protocol params reference UTxO (`ProgrammableLogicBase.hs:294`, `ProgrammableLogicBase.hs:299`) and per-proof directory reference nodes (`ProgrammableLogicBase.hs:218`, `ProgrammableLogicBase.hs:232`) | Protocol params CS anchor (`ProgrammableLogicBase.hs:299`), directory node CS validation (`ProgrammableLogicBase.hs:227`, `ProgrammableLogicBase.hs:244`) | Outputs at base credential must contain filtered programmable value (`ProgrammableLogicBase.hs:330`) | Programmable-token transfer logic script must be invoked for `TokenExists` proof branch (`ProgrammableLogicBase.hs:225`) |
| `mkProgrammableLogicGlobal` (`ProgrammableLogicBase.hs:284`) | Rewarding | `SeizeAct` (`ProgrammableLogicBase.hs:332`) | Issuer logic script withdrawal must be present (`ProgrammableLogicBase.hs:350`) | Protocol params reference (`ProgrammableLogicBase.hs:294`), directory node reference index in redeemer (`ProgrammableLogicBase.hs:341`) | Directory node CS check (`ProgrammableLogicBase.hs:352`) | For indexed programmable inputs, corresponding output address must match (`ProgrammableLogicBase.hs:533`); residual programmable deltas must remain in base outputs (`ProgrammableLogicBase.hs:500`) | Must provide as many relative indices as spending redeemers (`ProgrammableLogicBase.hs:354`); relative indices non-negative (`ProgrammableLogicBase.hs:356`) |

## State Graph

Primary state objects in scope:
- Protocol params UTxO (reference input) carrying `PProgrammableLogicGlobalParams` (`ProgrammableLogicBase.hs:303`, `ProgrammableLogicBase.hs:304`).
- Directory node UTxOs (reference inputs) carrying `PDirectorySetNode` (`ProgrammableLogicBase.hs:219`, `ProgrammableLogicBase.hs:221`, `ProgrammableLogicBase.hs:234`).
- Programmable mini-ledger UTxOs at base payment credential with stake-owner in staking credential (`ProgrammableLogicBase.hs:131`, `ProgrammableLogicBase.hs:133`).

Control assets:
- Protocol params CS identifies the protocol-params reference (`ProgrammableLogicBase.hs:299`).
- Directory node CS proves authenticity of referenced directory nodes (`ProgrammableLogicBase.hs:227`, `ProgrammableLogicBase.hs:244`, `ProgrammableLogicBase.hs:352`).

Transitions:
1. Base spend transition:
   - Spend at base validator requires global stake withdrawal presence (`ProgrammableLogicBase.hs:187`, `ProgrammableLogicBase.hs:196`).
2. Global `TransferAct` transition:
   - Collect value of base inputs whose owners are witnessed (`ProgrammableLogicBase.hs:124`, `ProgrammableLogicBase.hs:137`, `ProgrammableLogicBase.hs:142`).
   - Filter programmable token subset using directory proofs and transfer-script invocation (`ProgrammableLogicBase.hs:225`, `ProgrammableLogicBase.hs:226`).
   - Enforce programmable value remains under base credential (`ProgrammableLogicBase.hs:330`).
3. Global `SeizeAct` transition:
   - Iterate relative input indices over tx inputs; programmable selections must have same-address corresponding outputs (`ProgrammableLogicBase.hs:517`, `ProgrammableLogicBase.hs:533`).
   - Enforce output-side containment of computed programmable deltas (`ProgrammableLogicBase.hs:497`, `ProgrammableLogicBase.hs:500`).

Off-chain ordering assumptions impacting state transition validity:
- Global/base scripts are parameter-bound in directory environment (`Directory.hs:125`, `Directory.hs:126`).
- WST seize builder currently computes `plgrInputIdxs` from absolute tx input indices (`Wst .../BuildTx/ProgrammableLogic.hs:118`, `Wst .../BuildTx/ProgrammableLogic.hs:135`).

## Branch Cards

### Branch Card: `mkProgrammableLogicBase` delegation path

- Purpose:
  - Ensure any base-spend transaction also invokes the configured global stake validator (`ProgrammableLogicBase.hs:176`, `ProgrammableLogicBase.hs:196`).
- Inputs and assumptions:
  - Parameter `stakeCred` is the global stake credential (`OffChain/Scripts.hs:86`, `OffChain/Scripts.hs:88`).
  - Withdrawal map is non-empty when base spend is valid (`ProgrammableLogicBase.hs:187`).
- Outputs and effects:
  - No direct state mutation checks; returns success/failure only (`ProgrammableLogicBase.hs:196`).
- Block-by-block reasoning:
  - Read tx withdrawals (`ProgrammableLogicBase.hs:183`).
  - Fast-path compare first withdrawal credential (`ProgrammableLogicBase.hs:187`, `ProgrammableLogicBase.hs:189`).
  - Fallback search tail for matching credential (`ProgrammableLogicBase.hs:192`, `ProgrammableLogicBase.hs:194`).
- Cross-function continuity:
  - `stakeCred` is derived from global script hash by off-chain environment (`Directory.hs:140`, `Directory.hs:142`).
- Invariants:
  - I1: Base spend requires presence of global stake credential in withdrawals.
  - I2: No signature-only bypass exists in base script; gating is script-credential presence.
  - I3: Base script never independently authorizes token egress; delegated logic required.
- Assumptions:
  - A1: Ledger enforces script witness for script withdrawal entries.
  - A2: Off-chain code binds `stakeCred` to intended global script hash.
  - A3: Withdrawal map shape is canonical and stable.
  - A4: No chain-level path exists to inject withdrawal entries without corresponding witness.
  - A5: Runtime callers do not rely on base script for standalone business logic.

### Branch Card: `mkProgrammableLogicGlobal` `TransferAct`

- Purpose:
  - Enforce programmable-token transfer policy composition: owner witness + token-policy transfer script invocation + no programmable token escape from mini-ledger (`ProgrammableLogicBase.hs:311`, `ProgrammableLogicBase.hs:330`).
- Inputs and assumptions:
  - Protocol params reference input exists and contains expected datum (`ProgrammableLogicBase.hs:294`, `ProgrammableLogicBase.hs:304`).
  - Proof list aligns with programmable-token currency symbols in traversed input value (`ProgrammableLogicBase.hs:217`, `ProgrammableLogicBase.hs:252`).
- Outputs and effects:
  - Transaction valid only if output value at base credential contains required programmable value (`ProgrammableLogicBase.hs:330`).
- Block-by-block reasoning:
  - Resolve protocol params and global credential (`ProgrammableLogicBase.hs:299`, `ProgrammableLogicBase.hs:305`).
  - Compute witnessed base-input value (`ProgrammableLogicBase.hs:314`).
  - For each token proof, validate directory evidence and transfer-script invocation (`ProgrammableLogicBase.hs:225`, `ProgrammableLogicBase.hs:227`).
  - Enforce containment in base outputs (`ProgrammableLogicBase.hs:330`).
- Cross-function continuity:
  - Minting policy constrains mint destination to base credential first output (`Issuance.hs:115`, `Issuance.hs:130`).
- Invariants:
  - I1: Every spent base input requires stake-owner witness path.
  - I2: Every `TokenExists` proof requires corresponding transfer script withdrawal.
  - I3: Programmable value selected by proofs must remain under base credential outputs.
- Assumptions:
  - A1: Proof ordering and count are produced correctly off-chain.
  - A2: Directory node authenticity is fully represented by node CS presence checks.
  - A3: `pvalueContains` semantics align with intended containment for this value shape.
  - A4: Programmable token movement in tx outputs is complete in `ptxInfo'outputs` (no hidden sinks).
  - A5: Transfer-policy scripts correctly enforce policy-specific constraints beyond CIP layer.

### Branch Card: `mkProgrammableLogicGlobal` `SeizeAct`

- Purpose:
  - Allow issuer-authorized seizure while preserving mini-ledger conservation and per-input correspondence constraints (`ProgrammableLogicBase.hs:332`, `ProgrammableLogicBase.hs:349`).
- Inputs and assumptions:
  - Redeemer provides directory node index, relative input indices, outputs start index, length (`ProgrammableLogicBase.hs:332`, `ProgrammableLogicBase.hs:275`).
  - Issuer logic script credential is sourced from directory node datum (`ProgrammableLogicBase.hs:346`, `ProgrammableLogicBase.hs:350`).
- Outputs and effects:
  - For programmable indexed inputs: corresponding output address must match, programmable delta accumulated to residual base outputs (`ProgrammableLogicBase.hs:533`, `ProgrammableLogicBase.hs:535`, `ProgrammableLogicBase.hs:500`).
- Block-by-block reasoning:
  - Convert redeemer index list from data into integer list using fast unrolled map (`ProgrammableLogicBase.hs:334`, `ProgrammableLogicBase.hs:339`, `ProgrammableLogicBase.hs:385`).
  - Enforce length equals spending redeemer count (`ProgrammableLogicBase.hs:354`).
  - Traverse indices over remaining tx inputs (`ProgrammableLogicBase.hs:517`, `ProgrammableLogicBase.hs:524`).
  - Non-programmable selected script input: no-op, but selected pubkey input is rejected (`ProgrammableLogicBase.hs:542`, `ProgrammableLogicBase.hs:543`).
- Cross-function continuity:
  - Off-chain WST seize builder currently emits absolute indices (`Wst .../BuildTx/ProgrammableLogic.hs:119`, `Wst .../BuildTx/ProgrammableLogic.hs:135`).
- Invariants:
  - I1: Seize requires issuer-logic withdrawal invocation.
  - I2: Relative index list length must match number of spending redeemers.
  - I3: Relative indices must be non-negative.
- Assumptions:
  - A1: Off-chain builders emit relative index semantics expected on-chain.
  - A2: Output segment starting at `poutputsStartIdx` is the intended programmable continuation region.
  - A3: Script-spend counting helper (`penforceNSpendRedeemers`) matches ledger-purpose encoding assumptions.
  - A4: Directory node references are honest and in-scope of this policy instance.
  - A5: No external script mutates meaning of issuer logic credential binding.

## Compliance Matrix

| Spec claim | Spec source | Enforcing code path | Status | Gap notes |
|---|---|---|---|---|
| Base script forwards validation logic to global script | `doc/architecture.md:61` | `ProgrammableLogicBase.hs:187`, `ProgrammableLogicBase.hs:196` | Enforced | Delegation is credential-presence based. |
| Global script enforces programmable transfer policy composition | `doc/architecture.md:62` | `ProgrammableLogicBase.hs:225`, `ProgrammableLogicBase.hs:330` | Enforced | Depends on correct proofs and directory authenticity assumptions. |
| Programmable value remains in mini-ledger during transfer | `doc/architecture.md:62` | `ProgrammableLogicBase.hs:330` | Partially Enforced | Code TODO notes minted-value handling gap in this branch (`ProgrammableLogicBase.hs:312`). |
| Seize requires issuer logic authorization | `doc/architecture.md:11` | `ProgrammableLogicBase.hs:346`, `ProgrammableLogicBase.hs:350` | Enforced | Authorization delegated to issuer logic script witness. |
| Base/global script parameter linkage is deterministic from deployment env | `doc/architecture.md:51` | `Directory.hs:125`, `Directory.hs:142` | Enforced | Off-chain config integrity still operationally critical. |
| Seize index semantics are coherent end-to-end | implied operational requirement | On-chain expects relative (`ProgrammableLogicBase.hs:517`), WST off-chain emits absolute (`Wst .../BuildTx/ProgrammableLogic.hs:119`, `Wst .../BuildTx/ProgrammableLogic.hs:135`) | Not Enforced | Liveness/composability risk; tracked as area of interest. |

## Threat Boundary Table

| Actor | Controllable tx fields | Trust assumptions | Protected invariants | Worst-case capability in scope |
|---|---|---|---|---|
| Unprivileged user | Input/output selection for own UTxOs, withdrawal inclusion, redeemers for scripts they can witness | Cannot forge script witnesses or policy NFTs | No unauthorized spend of base-locked value without owner/issuer script paths | Can create failing transactions, attempt malformed proof/index data |
| Privileged operator / issuer | Issuer script invocation, seize orchestration, directory references chosen off-chain | Issuer script itself is trusted policy authority for seize actions | Seize must still preserve mini-ledger invariants and directory authenticity checks | Can seize per policy, can build malformed tx if off-chain index semantics diverge |
| Oracle / relayer | N/A in core CIP layer | Not used by on-chain programmable logic base/global checks | N/A | Minimal direct effect in scoped module |
| Off-chain integrator | Redeemer construction, reference/input ordering, output placement | Must implement index semantics and output segment conventions exactly | Correct proof/index encoding and ordering assumptions | Can cause systemic liveness failures or false negatives by mis-encoding txs |

## Context Closure Gate

- Result: PASS
- Gaps logged:
  - External semantic dependency on `pvalueContains` for delta containment with coerced value representation.
  - Off-chain WST seize builder appears semantically out-of-sync with relative index contract expectation.
- Why PASS despite gaps:
  - Entrypoints and redeemer branches in scoped target are fully mapped.
  - State/control-token continuity for scoped paths is documented with `file:line` evidence.
  - Assumptions and open questions are explicitly captured with impact.

## Areas of Interest

1. Minted-value interaction in `TransferAct` path is explicitly marked as TODO (`ProgrammableLogicBase.hs:312`); recommend targeted invariant tests combining mint + transfer in one tx.
2. WST off-chain seize builder currently computes absolute `plgrInputIdxs` while on-chain seize processing is relative (`Wst .../BuildTx/ProgrammableLogic.hs:119`, `Wst .../BuildTx/ProgrammableLogic.hs:135`, `ProgrammableLogicBase.hs:517`).
3. Delta containment path in seize uses `punsafeCoerce` into `PValue 'Sorted 'Positive` (`ProgrammableLogicBase.hs:499`); requires explicit test evidence for negative-delta edge cases.
