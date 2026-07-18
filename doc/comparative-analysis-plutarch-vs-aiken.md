# Comparative Analysis: Plutarch (wsc-poc) vs Aiken (cip113-programmable-tokens)

Date: 2026-07-17. Scope: on-chain scripts. Plutarch paths are relative to
`src/programmable-tokens-onchain/lib/`; Aiken paths relative to
`~/iohk/cip113-programmable-tokens/`.

## Executive summary

The two implementations share the same architecture (shared programmable-logic
base address + withdraw-zero global validator + sorted linked-list registry with
covering-node membership proofs + per-token substandard logic invoked via
withdraw-zero). The Plutarch version wins most CPU benchmarks (1.02–2.22×,
per `plutarch_vs_aiken_benches.txt`), but the Aiken version — having gone
through a July 2026 audit cycle — carries roughly ten audit-driven security
hardenings and two whole features we want (registry node updates, unfracking)
that the Plutarch version lacks. (Aiken also ships protected CIP-67 prefixes;
that feature is explicitly rejected here — see item 9.) The Plutarch seize path
also has one confirmed accounting hole (item 1) and relies on redeemer-supplied
input indices plus three compensating checks that the Aiken version proves
unnecessary (item 2).

Verification status: items 1 (seize accounting hole), 2 (input indexing), 6
(burn benchmark — corrected: Plutarch passes, Aiken fails) and 7 (registry-spend
own-key check) were verified directly against source by the coordinating
session. The remainder were produced by exhaustive parallel code reads of both
repos and carry file:line anchors for spot-verification during implementation.

---

## Ranked improvements (1 = highest impact)

### Tier 1 — Critical security fixes (Plutarch bugs / exploitable gaps)

> **STATUS: DONE & VERIFIED (2026-07-18).** Both drop-sites fixed
> (`psubtractTokens` output-exhausted branch + `goOuter` nil/mismatch branches,
> with `emitRemaining{Input,Output}ProgCS` helpers). Red→green tests added:
> `unit_seizeAct_full_seizure_to_pubkey_rejected`,
> `unit_seizeAct_partial_name_seizure_to_pubkey_rejected` (both failed pre-fix,
> pass post-fix), plus positive control
> `unit_seizeAct_full_seizure_to_base_output_succeeds`. Full suite 31/31 green.
> Benchmarks: Plutarch still beats Aiken on every seize scenario (CPU 1.13–1.23×
> after optimization). The fix's first cut regressed CPU (SeizeAct20 471M→631M)
> from per-pair `pfix` closure allocation in the helpers; replacing them with a
> non-recursive `remainingProgCSDelta` (a sorted value holds each policy at most
> once, so the leftover is one entry or none) recovered most of it (→584M). The
> remaining delta vs the pre-fix committed numbers is the *legitimate cost of
> correct accounting*: the old code was cheaper precisely because the drop-bug
> skipped work on fully-seized holdings. Not efficiency debt. Checkpoints:
> `doc/bench-checkpoint-after-item1.txt` (first cut),
> `doc/bench-checkpoint-item1-optimized.txt` (final).

**1. Fix silent value-drop in `pvalueEqualsDeltaCurrencySymbol` (seize accounting hole).**
`Contracts/ProgrammableLogicBase.hs:1425-1448` (`goOuter`). When the paired
output's value list exhausts before the input's (nil case at :1444) or the input
list exhausts (:1447), remaining currency entries are silently dropped instead
of being emitted as delta or rejected. Concretely: input `{ADA, progCS: 5}`
paired with output `{ADA}` — after ADA matches, the recursion hits the empty
output list and returns `pnil`, so the 5 seized tokens produce **no delta** and
the balance invariant (`checkBalanceInvariant`, :1190) is vacuously satisfied.
An issuer-authorized seize can therefore move programmable tokens out of the
base credential entirely (outputs before `poutputsStartIdx` are unconstrained).
Bounded by issuer-logic authorization, but it breaks the core "seized value
stays at the base address" invariant.

CONFIRMED (2026-07-18) by full-path trace — two co-located manifestations of the
same drop-bug: (a) `goOuter` nil branch (:1444) returns `pnil` when the paired
output has fully lost the progCS entry, dropping the positive delta; (b)
`psubtractTokens` output-exhausted nil branch (:1407) returns `inputRest`,
dropping the *current* input token-name, so seizing one name out of a multi-name
progCS holding also escapes. `checkBalanceInvariant` (:1191) then requires
containment of an empty/short delta → satisfied while tokens sit at a pubkey
output. Existing tests miss it: the burn test masks it (mint offsets to zero) and
every escape test keeps progCS in the paired output. Red test: input
`{ADA, progCS:5}`, base output `{ADA}`, pubkey output `{progCS:5}`, no
mint/burn → currently succeeds, must fail. Fix is two local branch corrections. Fix: adopt the Aiken structure —
`assets.split_at` at progCS on both sides, require byte-equality of the
before/after segments, and compute the delta only from the progCS segment
(cip113 `validators/programmable_logic/third_party.ak:186-229`); make every
list-exhaustion case either emit delta or `perror`, never silently accept.
Acceptance: a property test that any input/output pair differing outside progCS
fails, and full-seizure (output loses the CS entry) yields the correct positive
delta.

**2. Remove `pinputIdxs` redeemer indexing from SeizeAct — walk all inputs.**
`Contracts/ProgrammableLogicBase.hs:996-1020, 1179-1233`. The relative-index
machinery exists only to skip pubkey inputs, yet requires three compensating
checks (`penforceNSpendRedeemers` :1016, declared-length honesty :1018, per-index
credential classification :1129-1161) and off-chain relative encoding
(:836-846). The Aiken `third_party.ak` walks all inputs, classifying each by
payment credential in O(1), with no redeemer trust at all. Do the same: drop
`pinputIdxs`/`plengthInputIdxs` from `PSeizeAct`, iterate `txInfo.inputs` once,
pair every base-cred input with the next remaining output, skip others. Removes
redeemer parsing, `pasInt`, `pdropFast` per index, and two whole checks; strictly
shrinks the trust surface. Keep `pdirectoryNodeIdx` and `poutputsStartIdx`
(both verified-after-lookup and safe). Benchmark `SeizeAct1.ExternalScriptAnd50PubKeyInputs`
already exists to quantify the trade — expect a net win except in
pathological many-pubkey-input txs, and even there the cost is one constr-tag
check per input.

**3. Seize: forbid contaminating UTxOs that never held the seized policy (Aiken Finding 12).**
The Plutarch pairing accepts an input with zero seized-CS tokens whose output
*gains* them (negative delta, `psubtractTokens` :1399-1404 emits negative
entries which the containment check tolerates as extra output). Aiken requires
`expect !dict.is_empty(input_tokens_at)` per pair (`third_party.ak:225-229`).
Add the same guard: every paired base-cred input must actually hold the seized
CS. Prevents an issuer from injecting tokens into (and thereby perturbing)
innocent holders' UTxOs.

**4. Require an inline staking credential on issuance mint outputs.**
`Contracts/Issuance.hs:125-135` checks the first output's payment credential is
the base script and its value holds the minted amount, but never checks the
staking credential. A mint to the base cred with no (or a Pointer) staking
credential creates a UTxO whose owner check (`pvalueFromCred`
`ProgrammableLogicBase.hs:334`) `perror`s forever — permanently locked funds —
or worse is un-attributable. Aiken's `no_escape`/custody rule requires every
base-cred output to carry an `Inline` stake credential (`issuance_mint.ak:143-159`).
Add: minted-to output must have `SJust (StakingHash cred)`.

**5. Add a Certifying handler to the global (and all withdraw-zero) scripts permitting only registration.**
`Contracts/ProgrammableLogicBase.hs:982` accepts only the Rewarding purpose, so
any certificate naming the global stake credential fails. In Conway,
`RegCert` needs no script witness (so bootstrap works), but the current shape
means: (a) intent is implicit and fragile across ledger eras, and (b) if the
credential is ever registered via a pathway that later requires re-registration
after an unexpected dereg, there is no recovery. Mirror Aiken
(`programmable_logic_global.ak:79-84`): explicitly allow `RegisterCredential`,
fail `Delegate`/`Unregister`/vote-delegation (delegation abuse would let a third
party earn rewards against the shared credential or deregister it, bricking
every programmable token until re-registration). Apply the same to the
transfer-logic and issuer-logic example scripts and `unfracking` equivalent if
added (item 12).

**6. Confirm the full-burn semantics that Aiken fails (Plutarch's advantage — lock it in).**
`plutarch_vs_aiken_benches.txt:326-327` marks `programmableLogicMinting.Burn`
`[FAIL]` with `Plutarch PASS | Aiken FAIL` — i.e. the Plutarch burn path
*succeeds* and the Aiken equivalent errors on this scenario. This is a Plutarch
win worth protecting, not a bug. Verify *why* Aiken fails (likely its
empty-dict preservation on full burn, `assets.ak:141`, still fires the policy
proof but the benchmark tx shape trips a check) and ensure the Plutarch burn
branch (`Contracts/Issuance.hs:96-142` + global mint-proof path
`ProgrammableLogicBase.hs:747-805`, negative mint entries coerced to `Positive`
at :971) is genuinely correct and not merely permissive. Add a regression test
for full-burn-with-transfer-logic-invocation so this advantage cannot regress,
and confirm the merge (`ptokenPairsUnionFast`/`pcurrencyPairsUnionFast`
:149-234) keeps the policy entry so its transfer logic still runs on full burn.
Downgrade priority: this is verification/test-hardening, not a fix.

**7. Registry-spend must forbid minting/burning the spent node's own key (Aiken R-01).**
`LinkedList/SpendDirectory.hs:63-74` only requires *some* directory-node mint in
the tx. Nothing prevents the transaction from also minting/burning tokens of
the spent node's `key` (the programmable token policy) while the node is
mid-flight. Aiken adds `expect !mint_has_policy(self.mint, spent_node.key)`
(`registry_spend.ak:46-47`). Add the equivalent check to all directory-spend
variants.

**8. Genesis datum validation or an assertions build.**
All datums (`PProgrammableLogicGlobalParams` :628, `PDirectorySetNode` :682/:772/:1007,
`PBlacklistNode`, `PIssuanceCborHex`) are `punsafeCoerce`d. Node datums are
safe by induction (validated at creation), but the two genesis NFT datums
(protocol-params, issuance-cbor-hex) have **no on-chain shape check at mint
time** (`ProtocolParams.hs:24-41`, `IssuanceCborHex.hs:60-77`) — a malformed
genesis datum bricks or corrupts the whole system silently (Aiken audit D-01/D-02
territory, but Aiken at least fully deserializes the datum shape at mint,
`protocol_params_mint.ak`). Add: one-shot mints must find the NFT output and
fully deserialize its inline datum (`PTryFrom`), and pin the output to the
nonced always-fail address. Additionally add an assertions-enabled compilation
flag (mirroring Aiken's `env/with_assertions`) that upgrades hot-path
`punsafeCoerce` to checked decodes for testnet deployments.

### Tier 2 — Security hardening (defense-in-depth, audit parity)

**9. Protected CIP-67 prefixes in seize (Aiken Finding 18 + 20). — REJECTED (not pursuing).**
Decision (2026-07-17): not wanted. The feature (carving out CIP-67-labelled
tokens — reference NFTs, royalties — from issuer seizure via a
`protectedPrefixes` list on the node) adds datum size, an append-only update
constraint, and a size bound (was item 11) for no meaningful gain in this
system's threat model. Aiken's Findings 18/20 and the associated machinery are
intentionally **not** ported. Consequently: item 11 is dropped; item 10 (node
updates) no longer needs a `protected-prefixes` field; item 49's protected-name
interaction is removed.

**10. Registry node in-place updates.**
Plutarch registrations are immutable: `transferLogicScript`/`issuerLogicScript`
are fixed forever at insert, so a compromised or deprecated substandard script
can never be rotated — a serious operational/security limitation the Aiken
version solves (`registry_spend.ak:60-79` + `linked_list.ak:186`). Add an
update path to the directory spend validator: exactly one continuing output at
the same address holding the node NFT; `key`/`next`/minting-logic-cred frozen;
transfer-logic, issuer-logic and `globalStateCS` mutable (no protected-prefixes
field — item 9 rejected);
authorized by the node's minting-logic credential present in withdrawals; use
strict full-record reconstruction equality (NEW-1 pattern already used by
`pisInsertedOnNode`). Must compose with item 7's no-own-key-mint check.

**11. Bound `protectedPrefixes` size (Aiken M-01). — DROPPED (depends on rejected item 9).**
Only relevant if protected prefixes existed; since item 9 is rejected, there is
no unbounded-prefix datum to bound. Aiken's M-01 does not apply to the Plutarch
design. (Aiken's M-01 is a consequence of their having shipped the prefix
feature at all — by not shipping it, the Plutarch version avoids the finding
entirely rather than needing to fix it.)

**12. Unfracking (Aiken Finding 17).**
A freeze on one policy locks every co-located policy in the same UTxO
("fracked" UTxOs) — in Plutarch, a holder with a frozen token in a mixed UTxO
cannot even move their unfrozen tokens, because TransferAct runs every policy's
transfer logic. Port the Aiken standalone unfracking validator
(`validators/unfracking.ak`): a separate withdraw-zero credential recorded in
protocol params; invariants — zero mint, ≥1 base-cred input, single owner
(first base input's inline stake cred, authorized by sig or withdrawal; all
other base inputs and all base outputs must carry the same cred), strict
per-policy value equality at the base boundary, free datum/refscript choice.
Standalone keeps the global script small (Aiken measured ~600 bytes / quantified
lovelace saved per transfer). Add an `UnfrackingAct` arm to the global redeemer
that only checks the unfracking credential is in withdrawals. Note: requires
protocol-params datum extension → decide field layout before genesis (item 26).

**13. Blacklist non-membership proofs must also fail loudly on proof-list shape.**
`ExampleTransferLogic.hs:135-182` (`pvalidateWitnesses`) relies on `phead` of
an empty list erroring when proofs run short, and ignores *extra* proofs.
Require exact consumption (`[] == remaining` like Aiken's `verify_proofs`
final check, `transfer.ak:230-234`) so a malformed redeemer cannot mask a
missing witness under future refactors.

**14. Receiver-side screening example.**
`mkFreezeAndSeizeTransfer` checks only senders (staking creds of spent inputs).
A blacklisted party can receive tokens (then they're frozen in place — by
design, but OFAC-style compliance regimes typically require blocking receipt).
Both repos delegate this to substandards; ship an example transfer logic that
also proves non-membership for every *output* staking credential at the base
cred, so integrators have a vetted reference. (Feature-robustness over Aiken:
their platform repo has a denylist but the cip113 repo itself ships none.)

**15. Withdraw-zero amount checks (Aiken L-01, defense-in-depth).**
Neither implementation checks the withdrawal amount is 0 in forwarding checks.
Harmless today, but a shared observer credential accepting nonzero withdrawals
is a footgun if the credential ever accrues rewards. Low cost: in
`mkProgrammableLogicBase` and forwarding helpers, this is *not* worth per-entry
cost on the hot path — instead document the invariant and add it only to the
example substandard scripts where the scan already touches the entry.

**16. Directory registration should check mint quantity, not just presence.**
`LinkedList/Common.hs:277-282` (`_pisProgrammableTokenRegistration`) checks the
inserted CS appears in mint via `phasCS` — quantity/sign unchecked, so a
registration could accompany a *burn* of the token (weird but reachable only if
tokens pre-exist, which induction prevents; still, tighten to `> 0` or drop the
requirement entirely like Aiken, which deliberately allows register-without-mint
— pick one semantics and document it; register-without-mint is the more useful
feature, see item 34).

**17. Strip debug traces from production directory scripts.**
`LinkedList/Common.hs:222-226` carries `ptraceInfo`/`pshow` in `pInsert` —
script-size and CPU overhead on every registration, and `pshow` is notoriously
large. Gate behind the existing `Profile.hs`/tracing config so production
compilation is trace-free. Verify no other module leaks traces
(`grep ptraceInfo` across lib).

**18. Protocol-params lookup: use full membership, not first-entry, for the params NFT.**
`ProgrammableLogicBase.hs:597-603` (`phasCSHOrFalse`) matches the params UTxO
only if the params CS is the *first* non-Ada policy. Nothing on-chain
constrains the params UTxO value shape (item 8 fixes mint-time, but ADA-only +
NFT should also be enforced), so a params UTxO carrying an extra
lexicographically-smaller policy becomes invisible — availability failure of
the entire system. Either enforce exact value shape at mint (2 policies, like
`parseNodeOutputUtxo`), or use the full `phasDataCS` membership check the
directory spend already uses (`SpendDirectory.hs`). Prefer the mint-time shape
constraint — keeps the O(1) hot-path lookup sound.

### Tier 3 — Efficiency improvements

**19. Replace per-proof withdrawal scans with a pre-built checker.**
`pisScriptInvokedEntries` `ProgrammableLogicBase.hs:271-283` is O(#withdrawals)
per proof (adjacent-duplicate cache :706 helps only sorted-adjacent repeats).
Aiken builds a closure over the withdrawal list once (`transfer.ak:51-63`) and
pre-builds an indexed registry-node getter (`registry_node.ak:86`,
"pre-construct… to avoid repeatedly doing it in loops"). Port both: hoist a
`isInvoked :: Credential -> Bool` closure and an unrolled ref-input node getter
before the proof loop.

**20. Single-pass output containment via sorted merge.**
The multi-asset path of `poutputsContainExpectedValueAtCred`
(`ProgrammableLogicBase.hs:467-522`) aggregates value at cred (`pvalueToCred`,
one full output scan) then does `passetQtyInValue` — a fresh O(V) lookup per
expected asset → O(k·V). Both sides are canonically sorted; replace with one
O(V+k) zip like Aiken's `tokens.contains` (`lib/tokens.ak:16`) /
`pairs.pop_until` lockstep walk (`transfer.ak:174-234`).

**21. Blacklist proofs: switch `pelemAtFast` absolute indices to monotone relative indices.**
`ExampleTransferLogic.hs:136` re-traverses reference inputs from the head per
proof (O(k·n)). Reuse the relative-index/`pdropFast`-on-suffix technique the
seize path already uses (require ascending order, walk the suffix) → O(n+k).
Same for `ptransferProofs`/`pmintProofs` in the global validator if not already
suffix-based (:675, :765 use `pdropFast` from the head each time — make them
relative too).

**22. Hoist the seize per-pair output construction (existing TODO).**
`ProgrammableLogicBase.hs:67-69` TODO: instead of comparing address (:1147),
datum and refscript (:1149) field-by-field per pair, serialize-compare the
expected output built from the input (single `pdata` equality on the
reconstructed `TxOut` with substituted value), as Aiken effectively does with
three top-level field equalities on lazily-accessed fields. Benchmark both;
pick the cheaper (this is also where item 1's fix lands — do together).

**23. Lazy positional field access for hot-path datums.**
Aiken deliberately avoids full record deserialization, using positional
`head_list(tail_list(...))` accessors with single-walk reuse
(`params.ak:26-31`, `registry_node.ak:188` "partial traversal to index 4 is
reused, not recomputed"). Plutarch's raw-`Data` style is similar in places
(:311-316, :1129-1142) but `pletFields` full-record binds remain in the global
validator body. Audit each `pletFields` on `PDirectorySetNode`/params for
fields actually used per branch; switch to shared partial traversals.

**24. Mint-merge fast paths.**
Aiken's `apply_mint_to_known_policies` (`transfer.ak:140-166`) merges mint into
input assets with a monotone `split_at` (total O(inputs+mint)) and an empty-mint
fast path worth ~3.7 M CPU per policy. Verify `pcurrencyPairsUnionFast`
(:149-234) matches this bound and add the `mint == []` short-circuit before
entering `pcheckMintLogicAndGetProgrammableValue` (:966 area) if not present.

**25. Benchmark-gate every change.**
The repo already has the harness (`benchmark-onchain-compare.sh`,
`plutarch_vs_aiken_benches.txt`). Add a CI check that re-runs the comparison
and fails on >2% CPU/mem regression per scenario, and add missing scenarios:
full-burn, full-seizure-of-UTxO, unfracking,
node-update, 50-pubkey-input seize (exists), max-proofs transfer.

**26. Genesis-datum layout freeze with reserved fields.**
Items 10/12 extend datums (`PDirectorySetNode`, protocol params). Since
protocol params are immutable post-genesis (always-fail lock), design the final
field layout *once* — add `unfrackingCred`, and consider a reserved
`Data`-typed extension field to avoid future redeployments. Do this before any
mainnet genesis; it invalidates existing deployments (registry hashes change).

**27. Single-asset fast path for seize.**
The transfer path has a single-asset fast path (:453-467). The seize balance
check (`go2` :1198-1209) always aggregates full pair-lists; the overwhelmingly
common seize case is one policy, few token names. Add an early-exit
accumulate-until-quota variant symmetric with the transfer one.

**28. Unroll/specialize the base validator further.**
`mkProgrammableLogicBase` (:537-558) is already 2×-unrolled. It runs once per
programmable input, so bytes and CPU here multiply. Compare against Aiken's
28-line forwarder cost (their `pairs.has_key_or_fail` with no Option
allocation) in the benchmark; try 4× unroll and `pdelay`-free head/tail chains;
keep whichever wins the `NInputs` scaling benchmarks.

**29. Cache the first-withdrawal seed correctly.**
The transfer-logic cache is seeded with the *first withdrawal entry's
credential* (:948) — if the first withdrawal happens to be a proof's transfer
logic, the invocation check is skipped legitimately, but the seed choice is
arbitrary. Verify the seed cannot cause a false "already checked" for a
credential that is merely *present as key* but with the global cred itself
(it's in withdrawals by construction) — i.e. seed with the global credential
(guaranteed present and never a transfer logic? it could be!). Specify: seed
must be a credential that can never equal a registered transfer logic, or use
an explicit `PNothing` initial state. Small correctness-adjacent cleanup.

**30. Deduplicate directory/blacklist linked-list code.**
`LinkedList/Common.hs` vs `BlacklistCommon.hs` are near-mirrors. Factor the
shared node-output validation (2-policy check, key<next, TN==key, same-origin
address rule) into one parameterized module; halves audit surface and script
maintenance. (Compile-time only; no on-chain cost.)

**31. Remove the `plengthInputIdxs` field if item 2 is rejected.**
If input indexing is retained for any reason, the declared-length field is
redundant: compute the length in one pass (`pbuiltinListLengthFast` already
runs :1018) and use it for `penforceNSpendRedeemers` directly — one less
redeemer field to trust/validate.

**32. Adopt `pdropFast`-style suffix iteration for `pdirectoryNodeIdx` bounds.**
`phead # (pdropFast # idx # referenceInputs)` (:1000) errors on out-of-range —
fine — but each SeizeAct call re-derives the node; if multi-policy seize
(item 37) is added, use one sorted relative walk like the transfer proofs.

### Tier 4 — Feature parity and robustness (matching, then exceeding, Aiken)

**33. Issuance mint output flexibility (drop the first-output convention).**
`Issuance.hs:113-115` pins mints to the *first* output — a composability killer
(no batching with other protocols that claim output 0; two programmable-token
mints in one tx are impossible except via the shared-check trick). Aiken's
`issuance_mint` takes `OutputIndex { index }` or `RefInput { index }` redeemers
with a `no_escape` custody sweep (every output either at base-cred-with-inline-stake
or holds zero own tokens; `issuance_mint.ak:143-159`). Port this: redeemer
selects the registry-node proof; custody enforced by the sweep, not by
position. Subsumes item 4. Keep `psingleMintWithCredential` double-satisfaction
guard or replace with Aiken's precise-delegation equivalent (item 35).

**34. Register-without-mint.**
Plutarch requires the token be minted in the registration tx
(`Common.hs:280`). Aiken deliberately allows registration alone (issuers
prepare the registry entry before launch; types.ak:94-99). Adopt: drop the
mint-presence requirement — the blake2b preimage binding already guarantees
the key is the real issuance policy, and custody is enforced at actual mint
time. Simplifies item 16 away.

**35. Precise mint-delegation to the global validator (Aiken Finding 04/R-04).**
Aiken's issuance policy can delegate custody checking to the running global
validator only when the global redeemer *names the same registry node*
(decoding the PLG redeemer from `tx.redeemers`; `issuance_mint.ak:196-260`).
This enables mint-into-existing-transfer in one tx with no per-output sweep.
Port: in the issuance policy's ref-input branch, decode the global stake
script's redeemer and accept iff it is a `TransferAct` whose proofs include
this policy (or `SeizeAct` with matching node). This is the strongest known
fix for the mint-escape class; adopt wholesale.

**36. `globalStateCS` — implement or remove.**
The field exists in `PDirectorySetNode` but nothing reads it. Either wire it
(transfer logic receives a token-scoped global-state UTxO reference — e.g. for
supply caps, pause switches) with a documented convention, or drop the field
before freezing datum layout (item 26). A pause-switch example substandard
(read global-state NFT datum: `paused :: Bool`) would exceed Aiken's shipped
feature set.

**37. Multi-policy seize.**
Both implementations seize one policy per act (Aiken M-03 notes the CIP-113
spec wants skip-lists and richer acts). Extend `PSeizeAct` to a sorted list of
(node-proof, policy) pairs processed in one input walk — the per-pair delta
machinery generalizes by running `split_at` per policy against the same pair.
Neither repo has this; it makes batch compliance actions (court orders touching
several tokens) single-tx.

**38. CIP-113 spec alignment decision record.**
Aiken's M-03 enumerates divergences (7-field node, 30-byte sentinel, no
registration certificates, single proofs list, no input skip-list). Decide
explicitly which CIP-113 draft features the Plutarch version targets, and record
an ADR in `doc/`. Where the spec is better (skip-lists for seize — item 2's
walk-all approach actually *matches* Aiken not spec; document why), justify;
where the spec wins, schedule.

**39. Blacklist manager as credential, not pkh.**
`MintBlacklist.hs` gates insert/remove on a hard-coded manager *pubkey hash*.
A script credential (multisig/governance) cannot manage the blacklist. Accept a
`Credential` and check sig-or-withdrawal like the ownership check in
`pvalueFromCred`. Aiken delegates blacklists to substandards entirely, so this
plus item 14 gives Plutarch a more robust shipped compliance stack.

**40. Directory removal — deliberate non-feature, document it.**
Neither repo supports node removal (Aiken: no burn path by design; permanent
registry). Removing nodes breaks non-membership proofs for in-flight txs and
enables key re-registration games. Write the invariant down (doc + test that
asserts no redeemer can burn a directory NFT) rather than adding the feature.

**41. Blacklist `Remove` address/value hardening.**
`MintBlacklist.hs:225-265` (pRemove): the surviving covering node's output is
checked via three key equalities + `makeCommon`'s same-origin rule, but (unlike
Aiken's strict full-record reconstruction, NEW-1) the non-key fields of the
surviving node aren't pinned. Reconstruct the expected surviving node datum
byte-exactly (`covering with next := removedNext`) and compare, closing any
field-mutation-during-remove gap.

**42. Stranding tokens at a stake-less base-cred output. — ACCEPTED AS USER ERROR (no on-chain fix).**
Decision (2026-07-17): not a protocol vulnerability. `pvalueFromCred` :334
`perror`s on Pointer/Nothing staking creds at spend time, and today a transfer
*can* pay programmable tokens to a base-cred output with no staking credential
(`poutputsContainExpectedValueAtCred` counts outputs at base cred regardless of
staking cred, :351-353), stranding them. But this is the exact equivalent of a
user sending funds from a normal payment credential to a null/unspendable
address — user error at the wallet layer, not a hole in the validators. It is
not a griefing vector: an attacker can only strand *their own* value (transfer
requires every input's owner to authorize). No on-chain change; the burden is
on wallet/SDK UX to construct well-formed outputs. Documented here so it is a
recorded decision, not an oversight. (Note: item 4's inline-stake requirement on
*mint* outputs still stands — that path has no user in the loop choosing the
destination shape and a malformed genesis/mint would be a protocol defect.)

**43. Full deserialization of the blacklist node datum at insert.**
Mirror of item 8 for `BlacklistCommon`: verify `parseNodeOutputUtxo`-equivalent
structurally validates blacklist node datums (key length, cred shapes) as
strictly as the directory path (`pdeserializeDirectoryCredential` does for
directory nodes); add checks where missing.

**44. Property-based test suite over the script-context builder.**
The repo has `Test/ScriptContext/Builder.hs` and unit tests; Aiken has ~202
tests including pinned audit PoCs (`audit_findings.test.ak`). Port every Aiken
audit PoC (Findings 02, 04, 12, 13, 17, NEW-1, R-01 — excluding 18/20/M-01,
which relate to the rejected protected-prefix feature) as a
Plutarch test — each must fail against today's code where the gap exists and
pass after its item lands. This converts this document into an executable
checklist.

### Tier 5 — Beyond both implementations

**45. Upgradeable protocol parameters via governance, not redeployment.**
Both lock params at an always-fail address forever (upgrades = full
redeployment + token migration, which for the *registry* is impossible —
registered keys embed the old base cred). Design a v2: params NFT at a
governance-controlled spend script (threshold multisig / Agora-style), with the
global validator reading the *current* datum each tx. Trade-off analysis
required (a mutable base cred is impossible — payment cred is baked into every
UTxO — but transfer-logic-independent knobs like unfracking cred and fee params
can be mutable). Document what is and isn't upgradeable.

**46. Deployment ceremony automation + on-chain root authentication (Aiken D-01/D-02).**
Both trust the genesis datum contents. Build the acceptance-test tooling the
Aiken deployment report mandates: a `deploy-registry.sh`-integrated verifier
that recomputes every hash (base cred, global cred, node CS, cborhex
prefix/postfix vs actual compiled issuance template) from source and diffs
against the on-chain genesis datums before any registration is made; plus a
pinned `generated/` hash manifest in CI (Aiken's M-02 stale-blueprint lesson).

**47. Reference-script-aware seize/transfer size budget.**
Track total script bytes deployed as reference scripts and the per-tx reference
fee; the Aiken repo quantifies per-transfer lovelace cost of script size
(unfracking split rationale). Produce the same table for the Plutarch suite
(after item 17's trace stripping) and set size budgets per validator in CI.

**48. Formal invariant document + model checking of the linked list.**
Write down the registry induction invariants (sorted, unique keys, covering
completeness, sentinel bounds, address stability) and check them with a small
state-machine model (e.g. quickcheck-dynamic over the builder from item 44).
Both repos rely on these invariants implicitly across many validators; neither
has a machine-checked statement.

**49. Multi-token-name issuance policies.**
`Issuance.hs:107-109` restricts each policy instance to one token name per tx
(`pheadSingleton`). CIP-68-style tokens need paired names (100/222 labels) in
one mint. Relax to: all minted names of own CS must land at the base cred
output(s) (with item 33's sweep), quantity-summed.

**50. Interoperability test: Aiken substandards against Plutarch core.**
Both implementations expose the same architectural interface (withdraw-zero
credentials as substandard hooks). Add an integration test that runs the Aiken
repo's compiled transfer-logic/issuance substandards against the Plutarch
core (the repo already loads Aiken scripts —
`OffChain/AikenProgrammableTokenScripts.hs`), proving substandard portability
and pinning the ABI. This is the strongest form of "feature parity" evidence
and catches redeemer-shape drift automatically.

---

## Priority rationale

Tier 1 items are ordered by exploitability: item 1 is a verified accounting
hole in an authorized-but-privileged path; items 2–5 remove trust surface or
prevent permanent fund loss; 6–8 are correctness/bootstrap integrity. Tier 2
closes the gap to the audited Aiken feature set (items 9 and 11 are rejected —
see their entries). Two items previously flagged were closed as non-work by
user decision (2026-07-17): item 9 (protected CIP-67 prefixes — not wanted) and
item 42 (stake-less output stranding — accepted as wallet-layer user error).
Tier 3 protects the existing benchmark lead while paying for the new checks.
Tier 4 reaches feature parity then exceeds it (items 14, 36, 37, 39). Tier 5
is roadmap work neither implementation has.

Suggested first milestone (one sprint): items 1, 2, 3, 4, 7 (single PR — they
all touch the seize/transfer/mint value paths and share tests), plus item 44's
PoC tests to lock them in.
