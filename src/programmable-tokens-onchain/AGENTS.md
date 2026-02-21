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
