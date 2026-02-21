# Witness-Driven Complexity Collapse Finder (`onchain-contract` modules)

## Purpose

Find places where on-chain code is solving a search/selection problem, and collapse that complexity by having the redeemer provide a witness (usually indices), while on-chain code verifies correctness.

Key idea:
- Replace "search for the right element on-chain" with "witness points to candidate, validator verifies it."

This targets ExUnit reductions in hot paths, especially when:
- code scans lists and runs expensive predicates on many elements,
- the same list is traversed multiple times,
- datum/value decoding is repeated.

## Discovery

1. Scan for modules annotated with:
   `{-# ANN module ("onchain-contract" :: String) #-}`
2. In each module, inspect validator/policy entrypoints and helpers that touch:
   `ScriptContext`, `TxInfo`, inputs/outputs/reference inputs, datum/redeemer decoding, `Value`.

## Primary patterns

1. `find`/`filter`/fold-to-`Maybe` over inputs/outputs.
2. Multiple traversals over the same list.
3. Continuing output or own script input selection via scanning.
4. Repeated data decoding in hot helpers.
5. `flattenValue` search patterns where a direct check or one-pass witness check can be used.

## Rewrite strategy

1. Redeemer provides witness:
   - typically index/indices (absolute or relative, depending on design),
   - occasionally compact proof payloads.
2. On-chain selects exactly once.
3. On-chain verifies strong predicates that fully bind intended semantics.
4. Cache selected terms with `plet` and avoid duplicate list operations.

## Security and correctness rules

1. Witness-provided indices are acceptable when verified.
2. Out-of-bounds can be intentional fast-fail unless explicit bounds errors are desired.
3. Predicates must be binding (not weak checks like "has token X" only).
4. Do not assume cross-list positional alignment unless enforced.
5. If multiple elements could satisfy predicates, disambiguate via:
   - tx-shape constraints (preferred), or
   - explicit uniqueness checks.

## Performance rules

1. Optimize success path; do not add overhead solely to improve failure path.
2. Laziness has CEK overhead (`Delay`/`Force`); use it only when it helps honest-path cost.
3. Use CPS in hot paths when it removes intermediate structures/thunks and gives tighter evaluation control.
4. Avoid higher-order combinators in tight loops when a direct specialized traversal is cheaper.

## Module report template

```md
## Module: <ModuleName>
Path: <file>

### Candidate: <title>
Location: L<start>-L<end>
Current pattern: <search / repeated traversal / decode hotspot>
Why expensive: <predicate cost / repeated passes / conversions>

Witness-driven collapse:
- Redeemer provides: <indices/witness>
- On-chain selects: <single selection>
- On-chain verifies: <binding predicates>

Notes:
- Ambiguity risk: <none|details>
- Tx-shape constraints: <recommended checks>
- Expected ExUnits impact: <qualitative>
```

