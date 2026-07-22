# AGENTS.md

Repository-wide guidance for agents and contributors working in `wsc-poc`.
Package-specific notes live alongside their code (for example, the onchain
UPLC/Plutarch optimization log at
[`src/programmable-tokens-onchain/AGENTS.md`](src/programmable-tokens-onchain/AGENTS.md)).

## Workspace layout

This repository is a set of tightly-coupled packages whose dependencies flow
upward — each layer builds on the contract exposed by the one below it:

- **`programmable-tokens-onchain`** — Plutarch validators and minting policies,
  plus the redeemer/datum types and their on-chain encodings.
- **`programmable-tokens-offchain`** — transaction builders, environments, and
  script derivation that construct exactly those redeemers and datums.
- **`programmable-tokens-test` / `programmable-tokens-testlib`** — unit and
  property tests, golden encodings, and the on-chain benchmarks.
- **`src/examples/*`** (e.g. `regulated-stablecoin`) — example substandards
  built on the core protocol: their own offchain builders, executables, and
  integration tests.

Because the layers share types and encodings, a change to a lower layer is a
change to the contract every higher layer depends on.

## Keep the whole workspace consistent — never push a half-updated change

When you change a redeemer shape, datum layout, script parameter, exported
symbol, or any other cross-component contract, the change is **not finished**
when the package you edited compiles. It is finished when every package that
depends on it also compiles and passes against the new contract.

**Rule: before you push, or open or update a PR, if a change to one component
breaks any other component, you MUST update those other components in the same
change so that the entire workspace builds and all affected test suites pass.**

This applies in full to components you did not set out to touch — offchain
builders, example substandards, benchmark executables, and their tests are part
of the change the moment your edit breaks them. "It was already broken by an
earlier commit on this branch" is not an exception: a branch must not leave your
hands with a dependent package failing to compile or a dependent test red.

Concretely, when you touch onchain types or any shared contract:

1. Build **every dependent package**, not only the one you edited.
2. Update the offchain builders and environment, the example substandards, the
   executables, and the tests to match the new contract.
3. Run the affected test suites and confirm they are green before pushing.

If updating a dependent component is genuinely out of scope, do not silently
ship it broken: either descope the underlying change, or call out the remaining
breakage explicitly (in the PR description and to the user) rather than letting
a red tree look finished.

## Building and testing

Tests and benchmarks link libsodium's VRF, which lives in `/usr/local/lib`, so
build them with the extra library directory:

```
cabal build all --extra-lib-dirs /usr/local/lib
cabal test all --extra-lib-dirs /usr/local/lib
```

For onchain optimization work, follow the rules and record benchmark-backed
insights in [`src/programmable-tokens-onchain/AGENTS.md`](src/programmable-tokens-onchain/AGENTS.md).
