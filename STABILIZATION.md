# Stabilization Ledger

This is the live source of truth for search-lattice stabilization.

`JBH-refactor-notes.txt` is historical scratch and rationale only. If this file
and that file disagree, this file wins.

For the current L0-L3 language ordering and context-reuse graph, see
`racket-server/src/search-lattice/SEMILATTICE.md`.

During stabilization:
- no semantic edits above the current stabilization frontier unless needed to
  keep the repo compiling
- no new alpha renames unless a name itself causes a correctness bug or import
  failure
- if a higher layer compensates for a lower-layer defect, remove that
  compensation later rather than preserving it
- focused layer suites are the primary signal; umbrella/headless runs are
  secondary confirmation only
- legacy tests are evidence, not authority
- a failing legacy test must be triaged as one of:
  - intended semantic drift: rewrite or remove the test
  - stale harness assumption: narrow the test to the layer that still owns the
    claim
  - real regression: fix the implementation
- no pre-stabilization test is protected from deletion if it encodes an
  obsolete semantic story
- the active aggregate entrypoints stop at the current locked surface:
  `languages/all.rkt`, `reduction-relations/all.rkt`, `wf/all.rkt`, and
  `tests/test-all-headless.rkt` are intentionally limited to the current
  reopened surface
- quarantined L3+ code stays in-tree but is removed from active aggregate
  wiring until its lower-layer dependencies are locked
- prefer runtime/context factorizations that look like a good pre-image for a
  later refocusing+fusion abstract machine, rather than ad hoc grammar helpers
  or per-node scoped families

## Locked

- Trivial core utilities: `walk`, `unify`, `extend`, `occurs?`,
  `fresh-substitution`, `c-append`.

- L0/core runtime layer:
  `core-lang`, `core-red`, `core-wf`, and the focused L0 gate corpus in
  `racket-server/tests/stabilization-gates-tests.rkt`.
  Current touched-file inventory:
  `racket-server/src/search-lattice/languages/core-lang.rkt`,
  `racket-server/src/search-lattice/reduction-relations/core-red.rkt`,
  `racket-server/src/search-lattice/wf/core-wf.rkt`,
  `racket-server/tests/property-core.rkt`,
  `racket-server/tests/search-lattice-tests.rkt`,
  `racket-server/tests/stabilization-gates-tests.rkt`.
  Lock evidence:
  L0 now distinguishes `FreshenedTree` from `FreshenedShell`, `QFresh` owns the
  pure `FreshenedTree*` handoff role, `KLocal` is factored as nested
  conjunction layers over a `QFresh` bottom, and `core-red` owns the one final
  tree-to-shell lift for terminal tails.
  Phase note:
  `QFresh` is now the shared pure-prefix helper for scoped phase heads
  `(delay runnable-search)`, `(⊤ σ)`, and `(empty-tree)`, not just the L0
  conjunction-handoff witness.
  Empty-frame note:
  empty fresh frames are real runtime frames in the scoped semantics.
  `fresh ()` now steps to `FreshenedTree () ...`, shellification preserves that
  empty frame as `FreshenedShell () ...`, and the old
  `core/elide-empty-fresh` / `core/prune-empty-scope` story is retired.
  Lean-core note:
  core owns only the shell/tree role split and the local-work factoring it
  actually uses: `QShell`, `QFresh`, `KConj`, and `KLocal`.

- L1/delay runtime and wf layer:
  `delay-lang`, `delay-red`, `delay-wf`, and the focused L1 gate corpus in
  `racket-server/tests/stabilization-gates-tests.rkt`.
  Current touched-file inventory:
  `racket-server/src/search-lattice/languages/delay-lang.rkt`,
  `racket-server/src/search-lattice/reduction-relations/delay-red.rkt`,
  `racket-server/src/search-lattice/wf/delay-wf.rkt`.
  Lock evidence:
  nested-delay traces lock end-to-end, `Bounced` is introduced only at the
  delay frontier, and `delay/invoke-delay` is now the explicit L1 rule that
  turns a pure `FreshenedTree*` prefix around `delay` into a `FreshenedShell*`
  prefix around `Bounced`.
  Architecture note:
  `QShell` here is the committed shell path for `FreshenedShell` and
  `Bounced`.
  Grammar note:
  the real exclusion target for uninvoked `delay` is top-level already
  resolved search roots such as `(⊤ σ)`, `(empty-tree)`, and their
  shell/tree-freshened forms. `Bounced` and `(promoted + cfg)` are not the
  reason for the restriction; those are already `cfg`-only and not members of
  `search`.
  The active lower-lattice decomposition now reflects that directly:
  `search` carries unfinished `FreshenedTree` wrappers, `cfg` carries committed
  `FreshenedShell` wrappers, and `delay` remains a `search` form that wraps
  only `runnable-search`.

- L2/shared disjunction runtime and wf layer:
  `disj-lang`, `disj-base-red`, `disj-seq-red`, `disj-fused-red`,
  `disj-wf`, and the focused L2 gate corpus in
  `racket-server/tests/stabilization-gates-tests.rkt`.
  Current touched-file inventory:
  `racket-server/src/search-lattice/languages/disj-lang.rkt`,
  `racket-server/src/search-lattice/reduction-relations/disj-base-red.rkt`,
  `racket-server/src/search-lattice/reduction-relations/disj-seq-red.rkt`,
  `racket-server/src/search-lattice/reduction-relations/disj-fused-red.rkt`,
  `racket-server/src/search-lattice/wf/disj-wf.rkt`.
  Lock evidence:
  seq/fused differ only in their policy steps, shared-fresh and branch-local
  traces both complete, promoted left answers bubble to the spine in two steps,
  failures erase locally, and the shared L2 context grammar now carries both
  `KBranch` and `KLate`, with the seq/fused split living in the reducers.
  Architecture note:
  neutral L2 owns the shared branch zipper plus the committed-answer shell, and
  the disjunction frontier rules are now the layer-specific place where a pure
  `FreshenedTree*` prefix is committed into `FreshenedShell*`.

- L3/search-base runtime and wf layer:
  `search-base-lang`, `search-base-pre-red`,
  `search-base-seq-red`, `search-base-fused-red`,
  `search-base-wf`, and the focused L3 gate corpus in
  `racket-server/tests/stabilization-gates-tests.rkt`.
  Current touched-file inventory:
  `racket-server/src/search-lattice/languages/search-base-lang.rkt`,
  `racket-server/src/search-lattice/reduction-relations/search-base-pre-red.rkt`,
  `racket-server/src/search-lattice/reduction-relations/search-base-seq-red.rkt`,
  `racket-server/src/search-lattice/reduction-relations/search-base-fused-red.rkt`,
  `racket-server/src/search-lattice/wf/search-base-wf.rkt`.
  Lock evidence:
  seq/fused share one L3 language, plain L2 reassociation/consumption lifts
  unchanged into L3, the delay/disjunction shell-commit rules lift cleanly into
  the join, and the search-base reducers keep the same seq/fused policy split
  without reintroducing a generic shellification rule.

## Provisional

- Reopened calls/runtime overlays:
  `calls-lang`, `calls-red`,
  `search-base-calls-lang`,
  `search-base-*-calls-red`,
  `calls-wf`, `search-base-calls-wf`.
  Current status in the rebuild branch:
  active in `all.rkt`, `search-runtime.rkt`, `search-lattice-tests.rkt`, and
  `test-all-headless.rkt`, but still provisional pending broader app-facing
  reconnection.

- Reopened search-strategy/rail overlays:
  `search-dfs-*`,
  `search-flip-*`,
  `rail-lang`, `rail-calls-lang`,
  `rail-seq-red`, `rail-fused-red`,
  `rail-seq-calls-red`, `rail-fused-calls-red`,
  `rail-wf`, `rail-calls-wf`.
  Current status in the rebuild branch:
  active in `all.rkt`, the overlap audit, `search-runtime.rkt`, and
  `test-all-headless.rkt`, but still provisional pending downstream UI-facing
  integration.

- Downstream consumers:
  `canonical-json.rkt`,
  `contracts/visible-node-contract.json`,
  app/runtime-facing tests and visible/rendering expectations.
  Current status in the rebuild branch:
  quarantined from `test-all-headless.rkt`.

## Deficient

- `JBH-refactor-notes.txt` as an active spec. It is no longer authoritative.

- Delay/disjunction wf under-acceptance immediately after
  `FreshenedTree`-wrapped search steps.
  The lower reopened surface is fixed, but downstream layers still need the
  same audit whenever they introduce new `search` subforms.

- Host-side bubble/hoist helper logic in
  `racket-server/src/search-lattice/reduction-relations/private/common.rkt`.
  Search-base no longer depends on these helpers, but some reopened overlay
  layers still depend on other host-side helper logic there.

- Host-side scope/accounting helpers in
  `racket-server/tests/frontier-observable-support.rkt` where they exceed their
  role as test support and start acting as semantic authorities.

- Remaining quarantined layers still need to be propagated through the final
  `search` / `runnable-search` / branch-aware active-path factoring all the way to
  their final UI-facing consumers.

## Deferred Cleanup

- Possible shared post-L0 shell ancestor:
  if we later want a structural cleanup only, introduce a tiny common ancestor
  above `core` that defines only the outer `FreshenedShell*` shell
  ```
  [QShell ::= hole
              (FreshenedShell c QShell tag)]
  ```
  then let `delay` extend it with `Bounced` and neutral `disj` extend it with
  `(promoted + ...)`. This is currently treated as code-shape cleanup, not as a
  semantic restructuring.

## Policy Model

- The no-freshening policy model is now fixed:
  - one shared search-tree runtime grammar for seq and fused
  - one shared context grammar for seq and fused
  - incremental eager hoist for seq
  - late hoist for fused
  - the policy difference lives in the reduction layer, not in seq-specific
    runtime constructors or seq/fused context-language splits

- Consequence:
  some search-tree shapes are grammatical in the shared runtime language but
  unreachable under the seq policy. This is intentional. The distinguishing
  seq property is a reachability invariant, not a separate syntax class.

- Seq invariant:
  once an exposed `((alpha <-+ beta) × gamma)` appears on the active path, the
  next seq step must be the hoist. Seq may not make progress inside `alpha`
  first.

- Fused invariant:
  fused may keep descending on the active left path through both `<-+` and `×`;
  only once the left branch resolves does fused continue or erase at that
  boundary.

- Practical outcome:
  the shared helper grammar carries both `KBranch` and `KLate`, the reducers
  supply the seq/fused difference, and we are intentionally not adding either a
  separate seq-only pending-hoist runtime constructor or seq/fused context
  languages.

- Rejected alternative:
  a `HoistPending`-style boundary constructor in the runtime syntax. That would
  make the seq/fused distinction easier to encode syntactically, but at the
  cost of adding machinery to both policies and weakening the additive lattice
  story.

## Scope-Lifting Model

- Scope is now treated as an overlay on the no-freshening runtime skeleton, not
  as a second policy split.

- Operationally, every focused step does one of three things with the immediate
  scope prefix:
  - preserve `FreshenedTree*` for ordinary unfinished local work
  - carry `FreshenedTree*` across L0 conjunction handoff
  - reclassify `FreshenedTree*` to `FreshenedShell*` at shell-commit points

- The shell-commit points remain layer-local:
  - L0 final-tail completion
  - L1 `delay -> Bounced`
  - L2 disjunction reassociation/promotion/erasure

- Seq versus fused changes only where the active redex is found. It does not
  change the preserve/carry/reclassify story for scope prefixes.

## Frozen Renames

- `QFresh`
- `KBranch`
- `KLate`
- `wf-answer/core?`
- `calls-lang` should be renamed to `delay-calls-lang` when the calls overlay
  is reopened; the current name is historically inherited and semantically
  misleading because it already includes the delay layer
- language-provenance rule-name prefixes such as `core/...` and `delay/...`

No rename rollback is allowed during stabilization unless the name itself
causes a correctness bug or import failure.

## Lower-Layer Analysis

- Inherent after the `QFresh` split:
  `search`, `runnable-search`, `runnable-root`, `FreshenedTree`,
  `FreshenedShell`, `QFresh`, `QShell`, `KLocal`, `promoted`, `cfg`,
  `KBranch`, and `KLate`.
- Why `QFresh` is separate:
  it is the pure `FreshenedTree*` helper used by core scoped conjunction
  handoff, by the scoped delay/answer/fail phase rules, and by fused answer
  continuation.
- Why empty fresh frames remain:
  the scoped semantics is refining source fresh-frame structure, not only
  non-empty lvar introduction. So `FreshenedTree ()` is meaningful and should
  erase away, not be pruned by an extra administrative step.
- Why `QShell` is separate:
  it is the committed shell path over `FreshenedShell`, then extended by
  `Bounced` at L1 and `(promoted + ...)` at L2.
- Why `KBranch` remains necessary:
  it isolates the exposed left-branch boundary used by the seq hoist rule from
  inner local work. The witness is `((((a ∧ b) σ) <-+ (d σ)) × h c)`: seq must
  stop at that exposed boundary and hoist, rather than descending into
  `((a ∧ b) σ)` first.
- Why `KLate` remains necessary:
  it is part of the shared helper grammar, but only fused uses its extra
  descent power through `×`; seq’s restriction still lives in the reducer.
  On the same witness `((((a ∧ b) σ) <-+ (d σ)) × h c)`, fused may continue
  into `((a ∧ b) σ)` before the hoist boundary is discharged.
- Early vs late hoist remains an operational distinction, not an intended
  observable-answer distinction.
- Why there is no generic tree-prefix-to-shell step:
  it created real overlap with delay, disjunction, and rail policy steps.
  L0 owns only final-tail shellification; delay and disjunction own their
  layer-specific tree-prefix commitment rules.
