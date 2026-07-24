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
- `plet` has a cost. Use it when a value is reused or prevents expensive recomputation; avoid `plet` for single-use values where the binding overhead is not recovered. In cases where binding overhead is not worth it use normal haskell let instead. 
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
- Distinguish Haskell-level vs Plutarch-level arguments:
  - Haskell-level function args (e.g. `foo :: Term s a -> Term s b -> Term s a`) are metaprogramming. Applying them does not inherently create UPLC `Apply` nodes; it tends to inline/substitute at code generation time.
  - Plutarch-level args (e.g. `foo :: Term s (a :--> b :--> a)`) are actual on-chain lambda/application structure and do contribute runtime AST/application cost.
- Heuristic: prefer Haskell-level helpers when they are compile-time structure builders and do not duplicate large term fragments excessively.
- Heuristic: prefer Plutarch-level (`plam`, `:-->`) when you need real on-chain abstraction/reuse across call sites or must control evaluation/strictness at UPLC runtime.
- Heuristic: if a helper is called once in a hot path, Haskell-level specialization/inlining is often cheaper; if called many times with shared behavior, compare script-size growth vs runtime savings before deciding representation.

## 2026-03-19

- `processThirdPartyTransfer` in `ProgrammableLogicBase` is cheaper if the final balance-invariant accumulator tracks only token pairs for the seized `CurrencySymbol` instead of unioning full `Value`s from every remaining programmable output.
- Safety condition: keep the existing per-input/output `pvalueEqualsDeltaCurrencySymbol` check. That earlier check already proves all non-target policies are unchanged, so the final accumulation step only needs to prove containment for the target policy delta.
- Implementation pattern:
  - reuse a generalized `ptokensForCurrencySymbol` to project one sorted policy entry out of a `PValue`
  - accumulate those token pairs with `ptokenPairsUnionFast`
  - replace `pvalueContains` over a synthetic one-policy `Value` with a direct sorted-list containment check over token pairs
- Benchmark command:
  - `nix develop -c bash -lc 'dist=$(cabal list-bin benchmark-onchain-scripts); "$dist"'`
- Benchmark result against the original full-`Value` accumulator:
  - `programmableLogicGlobal.SeizeAct1`: CPU `62,680,817 -> 61,968,985` (`-711,832`, `-1.14%`), Mem `183,994 -> 179,963` (`-4,031`, `-2.19%`)
  - `programmableLogicGlobal.SeizeAct5`: CPU `152,303,493 -> 151,591,661` (`-711,832`, `-0.47%`), Mem `395,998 -> 391,967` (`-4,031`, `-1.02%`)
  - `programmableLogicGlobal.SeizeAct10`: CPU `259,367,892 -> 258,656,060` (`-711,832`, `-0.27%`), Mem `640,773 -> 636,742` (`-4,031`, `-0.63%`)
  - `programmableLogicGlobal.SeizeAct20`: CPU `470,967,642 -> 470,255,810` (`-711,832`, `-0.15%`), Mem `1,119,409 -> 1,115,378` (`-4,031`, `-0.36%`)
- `programmableLogicGlobal.SeizeAct1.ExternalScriptAnd50PubKeyInputs`: CPU `81,716,505 -> 81,555,330` (`-161,175`, `-0.20%`), Mem `239,107 -> 237,240` (`-1,867`, `-0.78%`)
- Full script-suite comparison showed no regressions; every changed row moved downward, including small fixed savings on some non-`SeizeAct` rows from the cheaper compiled `programmableLogicGlobal` script.

## 2026-03-20

- `TransferAct` does not need a sum-typed `TokenProof`. A witness list of reference-input indices is enough, because exact-match vs covering-node proof can be derived onchain from the referenced directory datum:
  - positive proof iff `nodeKey == currCS`
  - negative proof iff `nodeKey < currCS && currCS < nodeNext`
- Performance-sensitive control-flow detail: branch on `nodeKey < currCS` first, not on equality first. That preserves the old cheap negative-proof path and only pays the equality check on the non-negative path. Equality-first caused a small regression on the `TokenDoesNotExist` benchmark case.
- Offchain builder can also drop its own proof sum type and just emit the selected directory-node reference index.
- Benchmark command:
  - `nix develop -c bash -lc 'cabal build benchmark-onchain-scripts && dist=$(cabal list-bin benchmark-onchain-scripts); "$dist"'`
- Benchmark result against the previous `TokenProof` version:
  - `programmableLogicGlobal.TransferAct`: CPU `71,650,094 -> 70,310,138` (`-1,339,956`, `-1.87%`), Mem `203,044 -> 202,676` (`-368`, `-0.18%`)
  - `programmableLogicGlobal.TransferAct.MixedMany`: CPU `128,201,309 -> 122,931,127` (`-5,270,182`, `-4.11%`), Mem `375,396 -> 368,888` (`-6,508`, `-1.73%`)
  - `programmableLogicGlobal.TransferAct.Spend5Utxos`: CPU `190,571,194 -> 188,492,418` (`-2,078,776`, `-1.09%`), Mem `518,402 -> 515,100` (`-3,302`, `-0.64%`)
  - `programmableLogicGlobal.TransferAct.TokenDoesNotExist`: CPU `36,098,125 -> 35,359,305` (`-738,820`, `-2.05%`), Mem `110,764 -> 107,830` (`-2,934`, `-2.65%`)
  - `programmableLogicMinting.Burn`: CPU `98,089,703 -> 94,783,294` (`-3,306,409`, `-3.37%`), Mem `272,804 -> 269,870` (`-2,934`, `-1.08%`)
- Full script-suite comparison showed no regressions; `SeizeAct` rows were unchanged and every changed row moved downward.

## 2026-07-23

- Van Rossem (PV11) `dropList` builtin replaces every tail-walk drop in the hot
  paths: `pdropFast` sites in `ProgrammableLogicBase` (`pparamsAtRefIdx`, the
  transfer-proof and `NonMember` covering-node fetches, the seize
  `outputsStartIdx`/`directoryNodeIdx` drops) and the loop inside `pcheckedDrop`
  in `Issuance` (the negative-index guard stays: the builtin clamps negative
  counts to zero instead of erroring, and the §14.1 red test pins rejection).
- Isolation benchmark (`benchmark-onchain-functions`, `decision.d.drop.*`,
  300-element list): builtin vs tail-loop CPU 552,595 vs 3,121,220 at n=1 up to
  1,049,673 vs 43,315,600 at n=255 (5.6x-41x); builtin memory flat 2,100 vs
  13,940-124,024. Cost function: CPU 116,711 + 1,957/element, mem constant 4.
- Full-suite result vs the post-bump baseline (validators otherwise identical):
  zero regressions; ManyPolicies40 -32.5% CPU / -42.9% mem, MixedMany -14.4% /
  -21.3%, Mint -12.5% / -18.9%, SeizeAct1 -7.5% / -12.0%, TransferAct -5.0% /
  -7.7%. Projected Policies dimension maximum at the 14M mainnet mem limit rose
  from 220 to 410. The two previously-behind-Aiken rows flipped:
  Mint.BusyTx20Outputs mem 0.98x -> 1.04x, SeizeAct1.External50 mem 0.96x ->
  1.00x; only the untouched directoryNodeMinting scenarios remain behind.
- Adoption note: indices that feed `dropList` directly are self-validating
  hints backed by authenticated checks (params NFT, covering-node NFT+interval,
  containment pairing), so negative-clamps-to-zero grants no new power; keep an
  explicit guard only where negative rejection is contract behaviour.
