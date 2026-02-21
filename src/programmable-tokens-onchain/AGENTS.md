# Onchain UPLC/Plutarch Optimization Log

This file is the running optimization memory for `programmable-tokens-onchain`.
Update it whenever a benchmark-backed optimization insight is discovered.

## Optimization Rules

- Keep security invariants first. Do not trade away safety checks for lower ex-units.
- Keep tracing calls in contracts. Benchmark with `NoTracing` config instead of deleting traces.
- Optimize for the success path (honest transaction path); failed-path cost is secondary.
- Never optimize for failing executions. Failure-path ExUnit cost is not an optimization target; do not trade any honest-path efficiency to improve it.
- Prefer witness-driven complexity collapse: pass compact witnesses/redeemer data to avoid onchain global searches.
- Prefer verification over solving: witness supplies candidate(s), script verifies strong binding predicates.
- Avoid full tx-input scans when an index/witness can prove the same fact.
- Avoid higher-order combinators in hot paths when a direct specialized loop is cheaper.
- `plet` has a cost. Use it when a value is reused or prevents expensive recomputation; avoid `plet` for single-use values where the binding overhead is not recovered.
- Validate list lengths/coverage in lockstep traversals to avoid ambiguous or partial proofs.

## Proven Patterns

- Relative indices for repeated input selection reduce repeated traversal overhead vs absolute indexing from list head each time.
- For known-length data conversions, length-guided unrolling can help, but only if it avoids duplicated list-tail work.
- Unrolling that duplicates traversal (e.g., repeated tails for both conversion and recursion state) often yields only marginal gains.
- Lockstep traversal over two ordered lists (data + witness) is typically cheaper than lookup structures/maps when ordering is already available.
- Branch-local checks can be structured to favor the success path; `pand'List` can outperform chained `#&&` in some success-heavy paths when one branch is effectively error-only.
- Laziness has a real CEK cost (`Delay`/`Force`). For long guard chains where success is expected and failure should abort anyway, prefer success-path-cheaper strict/linear check structures over per-link lazy short-circuit structures; do not accept extra success-path cost to reduce failure-path cost.
- CPS is often a useful tool in hot paths to control evaluation order and avoid building intermediate structures/thunks when sequencing validations.
- Witness-driven index selection is valid and desirable when paired with strong verification predicates; ambiguity must be eliminated by tx-shape constraints or explicit uniqueness checks.
- Avoid positional coupling assumptions across different lists (e.g. `inputs[i]` corresponds to `outputs[i]`) unless enforced by predicates/shape constraints.
- Distinguish Haskell-level vs Plutarch-level arguments:
  - Haskell-level function args (e.g. `foo :: Term s a -> Term s b -> Term s a`) are metaprogramming. Applying them does not inherently create UPLC `Apply` nodes; it tends to inline/substitute at code generation time.
  - Plutarch-level args (e.g. `foo :: Term s (a :--> b :--> a)`) are actual on-chain lambda/application structure and do contribute runtime AST/application cost.
- Heuristic: prefer Haskell-level helpers when they are compile-time structure builders and do not duplicate large term fragments excessively.
- Heuristic: prefer Plutarch-level (`plam`, `:-->`) when you need real on-chain abstraction/reuse across call sites or must control evaluation/strictness at UPLC runtime.
- Heuristic: if a helper is called once in a hot path, Haskell-level specialization/inlining is often cheaper; if called many times with shared behavior, compare script-size growth vs runtime savings before deciding representation.

## Current Findings (bench-backed)

### 2026-02-20

- TransferAct proof encoding comparison:
  - Variant tested: replace typed proofs (`TokenExists`/`TokenDoesNotExist`) with index-only proofs and infer inclusion/non-inclusion from node relation (`key == cs` vs `key < cs < next`).
  - Result after optimization pass:
    - `TransferAct`: improved CPU slightly.
    - `TransferAct.MixedMany`: improved CPU more noticeably.
    - `TransferAct.TokenDoesNotExist`: regressed slightly.
    - Memory generally increased slightly.
  - Conclusion: index-only proof encoding is not universally better; evaluate per workload shape.

- `pand'List` preference on success-heavy branches:
  - Switching selected conjunction sites from `#&&` to `pand'List` in success-dominant logic reduced CPU and memory modestly in TransferAct benchmark paths.

- `TransferAct` mint split fused into one pass:
  - Variant tested: replace separate `pselectPositiveMintValue` / `pselectBurnAbsMintValue` traversals with one fused pass returning both values.
  - Result:
    - `TransferAct`: CPU -436,994 (-0.402%), Mem -1,932 (-0.564%).
    - `TransferAct.MixedMany`: CPU -436,994 (-0.228%), Mem -1,932 (-0.343%).
    - `TransferAct.TokenDoesNotExist`: CPU -436,994 (-0.651%), Mem -1,932 (-0.897%).
  - Conclusion: keep; consistent TransferAct improvement with no regressions in this benchmark suite.

- Mint-proof `TokenExists` conjunction switched to `pand'List`:
  - Variant tested: replace one `#&&` chain with `pand'List` in mint-proof `TokenExists` checks.
  - Result:
    - No measurable ExUnit change across benchmarked paths.
  - Conclusion: neutral on this suite.

- Add `@5` branch in `pmapBuiltinListDataToIntegerFast`:
  - Variant tested: add length-5 unrolled conversion branch between `@10` and fallback in seize index decoding.
  - Result:
    - `SeizeAct5`: CPU -1,422,258 (-0.770%), Mem -5,360 (-1.034%).
    - `SeizeAct1`: CPU +295,886 (+0.372%), Mem +1,102 (+0.468%).
    - `SeizeAct10`: CPU +295,886 (+0.095%), Mem +1,102 (+0.129%).
    - `SeizeAct20`: CPU +295,886 (+0.053%), Mem +1,102 (+0.073%).
  - Conclusion: mixed; branch helps exact-length-5 case while adding small overhead to other seize sizes.

- TransferAct containment with `punsafeCoerce` of signed sum to `Positive`:
  - Variant tested: replace directional mint/burn containment rewrite with:
    - RHS = `punsafeCoerce @(PValue 'Sorted 'Positive) ((punsafeCoerce @(PValue 'Sorted 'NoGuarantees) totalProgTokenValue_) #<> programmableMintSignedValue)`
    - Check = `pvalueContains outputsProg RHS`
  - Result (vs CPS directional split variant):
    - `TransferAct`: CPU -3,694,589 (-3.416%), Mem -15,348 (-4.514%).
    - `TransferAct.MixedMany`: CPU -7,043,815 (-3.683%), Mem -27,188 (-4.849%).
    - `TransferAct.TokenDoesNotExist`: CPU -1,774,982 (-2.667%), Mem -8,696 (-4.083%).
    - Seize paths also changed slightly from global script-size effects (small deltas).
  - Conclusion: materially cheaper in benchmark suite; correctness depends on relying on external invariants for positivity of the signed sum.

### 2026-02-21

- TransferAct no-mint fast path:
  - Variant tested: when `txInfo.mint` is empty, skip `pcheckMintLogicAndGetProgrammableValue` and require `mintProofs == []` (fail on extra mint proofs).
  - Why it helped: avoids mint-proof traversal and signed-value merge in the common no-mint transfer path.
  - Security impact: preserved and slightly tightened in no-mint transactions by rejecting redundant mint proofs.
  - Keep / revert: keep.

- Strip Ada before accumulation in transfer containment path:
  - Variant tested:
    - In `pvalueFromCred`, accumulate `pstripAdaH inputValue` instead of full input value.
    - In `pvalueToCred`, accumulate `pstripAdaH outputValue` per matched output instead of stripping once at the end.
    - In transfer proof filtering, stop dropping the first currency-symbol entry (because values are already Ada-stripped).
  - Why it helped: removes repeated add/remove work for Ada and reduces intermediate value-map size in hot transfer checks.
  - Security impact: preserved; non-Ada programmable-value containment invariant is unchanged.
  - Keep / revert: keep.

- Protocol-params reference lookup first-hit specialization:
  - Variant tested: specialize `pfindReferenceInputByCS` to check the first reference input directly, then fall back to recursive search; also share resolved outputs with `plet`.
  - Why it helped: many benchmarked contexts place protocol params first, so this removes one recursive step and avoids duplicate resolved-output extraction.
  - Security impact: preserved; same datum/token validation and same fallback behavior.
  - Keep / revert: keep.

- Aggregate benchmark deltas from this pass (vs pre-pass baseline):
  - `programmableLogicGlobal.TransferAct`: CPU `95,211,692 -> 80,851,236` (`-14,360,456`, `-15.08%`), Mem `303,606 -> 246,909` (`-56,697`, `-18.68%`).
  - `programmableLogicGlobal.TransferAct.MixedMany`: CPU `158,720,713 -> 150,930,949` (`-7,789,764`, `-4.91%`), Mem `480,214 -> 439,167` (`-41,047`, `-8.55%`).
  - `programmableLogicGlobal.TransferAct.TokenDoesNotExist`: CPU `55,876,604 -> 50,408,335` (`-5,468,269`, `-9.79%`), Mem `183,862 -> 162,699` (`-21,163`, `-11.51%`).
  - `programmableLogicBase.Tx.d29ce2a9.Stake`: CPU `96,494,338 -> 82,133,882` (`-14,360,456`, `-14.88%`), Mem `307,770 -> 251,073` (`-56,697`, `-18.42%`).

## Plutus-Core Costing Internals (v1.51.0.0)

- Dependency pins in this repo:
  - `plutus-core == 1.51.0.0`
  - `plutus-ledger-api == 1.51.0.0`
  - `plutarch == 1.12.0`
- Cost model source of truth in plutus-core:
  - `cost-model/data/builtinCostModel[A|B|C].json`
  - `cost-model/data/cekMachineCosts[A|B|C].json`
  - Testing defaults use Variant C.
- CEK machine step charging (Variant C):
  - `cekStartupCost`: CPU 100, Mem 100.
  - `cekVar/Const/Lam/Delay/Force/Apply/Builtin/Constr/Case`: CPU 16000, Mem 100 each.
  - Practical implication: reducing AST node count (especially `Apply`, `Force`, `Delay`, `Var`) is always valuable in hot paths.
- UPLC strictness/laziness model:
  - CEK application is call-by-value on term arguments (`Apply` evaluates function and argument before `applyEvaluate`).
  - Laziness is explicit via `Delay`/`Force`.
  - Practical implication: branch payloads must be delayed to avoid paying both branches; avoid introducing unnecessary `force/delay` pairs in hot loops.
- Builtin costing model shape (Variant C has 92 builtins):
  - 43 builtins use constant CPU cost.
  - Others are size-sensitive (e.g. `linear_in_x`, `min_size`, `max_size`, `added_sizes`, `quadratic`, etc).
  - Memory cost is constant for most builtins (66/92 constant).
- High-signal builtin costs for contract engineering:
  - Structural list/data helpers are not free:
    - `headList` 83,150 CPU
    - `tailList` 81,663 CPU
    - `nullList` 74,433 CPU
    - `mkCons` 72,362 CPU
    - `chooseList` 132,994 CPU
  - `equalsData` is `min_size` with high slope (`intercept=898,148`, `slope=27,279`), so deep/large data equality is expensive.
  - `serialiseData` is `linear_in_x` with very high slope (`213,312`), avoid in hot validation paths.
  - Cryptographic builtins can dominate total cost by orders of magnitude (e.g. BLS final verify ~333,849,714 CPU constant).
- CEK implementation-level performance facts (relevant when vendoring/patching evaluator internals):
  - Strictness changes can materially affect performance:
    - `StrictData` in CEK internals made evaluator slower (commented in source).
    - Making the discharged term in `VBuiltin` strict caused ~3-4.5% slowdown.
  - CEK uses budget "slippage" batching (default 200 machine steps) to reduce per-step accounting overhead.
- Cost model parameter application:
  - Ledger-facing params are flattened name->int maps; CEK params are split by `cek` prefix.
  - Applying params is expensive; compute once and cache evaluation contexts/machine parameters.

## Benchmark Discipline

- Always run:
  - `cabal build benchmark-onchain-scripts`
  - `benchmark-onchain-scripts` executable (NoTracing path)
- Record deltas for at least:
  - `programmableLogicGlobal.TransferAct`
  - `programmableLogicGlobal.TransferAct.TokenDoesNotExist`
  - `programmableLogicGlobal.TransferAct.MixedMany`
- Keep before/after numbers with CPU and memory, plus percent deltas.

## Update Template

Use this format for new entries:

- Date:
- Change:
- Why it should help:
- Benchmark deltas (CPU/Mem, abs + %):
- Security impact:
- Keep / revert decision:
