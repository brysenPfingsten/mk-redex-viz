# Stabilization Ledger

This is the live source of truth for the active search-lattice stabilization
work. It is a contributor ledger, not the primary reader-facing semantics
overview for the repo as a whole.

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
- future presentation note:
  if we later want a smaller top-level reducer inventory, localize raw
  layer-specific `.../base` delta relations consistently across the lattice
  instead of doing so piecemeal
- union-surface note:
  pre-union reducer pieces at the module that creates them only when every
  real downstream use consumes the same whole bundle as one summand.
  Keep pieces separate when later layers route the constituents through
  different closures, lifts, or policy cuts, and do not export overlapping
  convenience bundles such as `(A ∪ B)` and `(B ∪ C)` unless a real consumer
  actually wants each exact bundle.

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
  L0 now distinguishes `ScopedTree` from `ScopedShell`, `FreshCtx` owns the
  pure `ScopedTree*` handoff role, `LocalCtx` is factored as nested
  conjunction layers over a `FreshCtx` bottom, and `core-red` owns the one final
  tree-to-shell lift for terminal tails.
  Phase note:
  `FreshCtx` is now the shared pure-prefix helper for scoped phase heads
  `(delay runnable-search)`, `(⊤ σ)`, and `(empty-tree)`, not just the L0
  conjunction-handoff witness.
  Empty-frame note:
  empty fresh frames are real runtime frames in the scoped semantics.
  `fresh ()` now steps to `ScopedTree () ...`, shellification preserves that
  empty frame as `ScopedShell () ...`, and the old
  `core/elide-empty-fresh` / `core/prune-empty-scope` story is retired.
  Lean-core note:
  core owns only the shell/tree role split and the local-work factoring it
  actually uses: `ShellCtx`, `FreshCtx`, `ConjCtx`, and `LocalCtx`.

- L1/delay runtime and wf layer:
  `delay-lang`, `delay-red`, `delay-wf`, and the focused L1 gate corpus in
  `racket-server/tests/stabilization-gates-tests.rkt`.
  Current touched-file inventory:
  `racket-server/src/search-lattice/languages/delay-lang.rkt`,
  `racket-server/src/search-lattice/reduction-relations/delay-red.rkt`,
  `racket-server/src/search-lattice/wf/delay-wf.rkt`.
  Lock evidence:
  nested-delay traces lock end-to-end, `Deferred` is introduced only at the
  delay frontier, and `invoke-delay` is now the explicit L1 rule that
  turns a pure `ScopedTree*` prefix around `delay` into a `ScopedShell*`
  prefix around `Deferred`.
  Architecture note:
  `ShellCtx` here is the committed shell path for `ScopedShell` and
  `Deferred`.
  Grammar note:
  the real exclusion target for uninvoked `delay` is top-level already
  resolved search roots such as `(⊤ σ)`, `(empty-tree)`, and their
  shell/tree-freshened forms. `Deferred` and `(answers + cfg)` are not the
  reason for the restriction; those are already `cfg`-only and not members of
  `search`.
  The active lower-lattice decomposition now reflects that directly:
  `search` carries unfinished `ScopedTree` wrappers, `cfg` carries committed
  `ScopedShell` wrappers, and `delay` remains a `search` form that wraps
  only `runnable-search`.

- L2/shared disjunction runtime and wf layer:
  `disj-lang`, `disj-base-red`, `disj-early-red`, `disj-late-red`,
  `disj-wf`, and the focused L2 gate corpus in
  `racket-server/tests/stabilization-gates-tests.rkt`.
  Current touched-file inventory:
  `racket-server/src/search-lattice/languages/disj-lang.rkt`,
  `racket-server/src/search-lattice/reduction-relations/disj-base-red.rkt`,
  `racket-server/src/search-lattice/reduction-relations/disj-early-red.rkt`,
  `racket-server/src/search-lattice/reduction-relations/disj-late-red.rkt`,
  `racket-server/src/search-lattice/wf/disj-wf.rkt`.
  Lock evidence:
  early/late differ only in their policy steps, shared-fresh and branch-local
  traces both complete, left answers bubble to the spine in two steps,
  failures erase locally, and the shared L2 context grammar now carries both
  `BranchCtx` and `LateCtx`, with the early/late split living in the reducers.
  Architecture note:
  neutral L2 owns the shared branch zipper plus the committed-answer shell, and
  the disjunction frontier rules are now the layer-specific place where a pure
  `ScopedTree*` prefix is committed into `ScopedShell*`.

- L3/search runtime and wf layer:
  `search-lang`, `search-pre-red`,
  `search-early-red`, `search-late-red`,
  `search-wf`, and the focused L3 gate corpus in
  `racket-server/tests/stabilization-gates-tests.rkt`.
  Current touched-file inventory:
  `racket-server/src/search-lattice/languages/search-lang.rkt`,
  `racket-server/src/search-lattice/reduction-relations/search-pre-red.rkt`,
  `racket-server/src/search-lattice/reduction-relations/search-early-red.rkt`,
  `racket-server/src/search-lattice/reduction-relations/search-late-red.rkt`,
  `racket-server/src/search-lattice/wf/search-wf.rkt`.
  Lock evidence:
  early/late share one L3 language, plain L2 reassociation/consumption lifts
  unchanged into L3, the delay/disjunction shell-commit rules lift cleanly into
  the join, and the search reducers keep the same early/late policy split
  without reintroducing a generic shellification rule.

## Provisional

- Reopened relcall/runtime overlays:
  `relcall-lang`, `relcall-red`,
  `search-relcall-lang`,
  `search-*-relcall-red`,
  `relcall-wf`, `search-relcall-wf`.
  Current status in the rebuild branch:
  active in `all.rkt`, `search-runtime.rkt`, `search-lattice-tests.rkt`, and
  `test-all-headless.rkt`, but still provisional pending broader app-facing
  reconnection.

- Reopened search-strategy/rail overlays:
  `search-dfs-*`,
  `search-flip-*`,
  `rail-lang`, `rail-relcall-lang`,
  `rail-early-red`, `rail-late-red`,
  `rail-early-relcall-red`, `rail-late-relcall-red`,
  `rail-wf`, `rail-relcall-wf`.
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
  `ScopedTree`-wrapped search steps.
  The lower reopened surface is fixed, but downstream layers still need the
  same audit whenever they introduce new `search` subforms.

- Host-side bubble/hoist helper logic in
  `racket-server/src/search-lattice/reduction-relations/private/common.rkt`.
  Search no longer depends on these helpers, but some reopened overlay
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
  above `core` that defines only the outer `ScopedShell*` shell
  ```
  [ShellCtx ::= hole
                (ScopedShell c ShellCtx tag)]
  ```
  then let `delay` extend it with `Deferred` and neutral `disj` extend it with
  `(answers + ...)`. This is currently treated as code-shape cleanup, not as a
  semantic restructuring.

## Policy Model

- The no-freshening policy model is now fixed:
  - one shared search-tree runtime grammar for early and late
  - one shared context grammar for early and late
  - incremental eager hoist for early
  - late hoist for late
  - the policy difference lives in the reduction layer, not in early-specific
    runtime constructors or early/late context-language splits

- Consequence:
  some search-tree shapes are grammatical in the shared runtime language but
  unreachable under the early policy. This is intentional. The distinguishing
  early property is a reachability invariant, not a separate syntax class.

- Early invariant:
  once an exposed `((alpha <-+ beta) × gamma)` appears on the active path, the
  next early step must be the hoist. Early may not make progress inside `alpha`
  first.

- Late invariant:
  late may keep descending on the active left path through both `<-+` and `×`;
  only once the left branch resolves does late continue or erase at that
  boundary.

- Practical outcome:
  the shared helper grammar carries both `BranchCtx` and `LateCtx`, the reducers
  supply the early/late difference, and we are intentionally not adding either a
  separate early-only pending-hoist runtime constructor or early/late context
  languages.

- Rejected alternative:
  a `HoistPending`-style boundary constructor in the runtime syntax. That would
  make the early/late distinction easier to encode syntactically, but at the
  cost of adding machinery to both policies and weakening the additive lattice
  story.

## Scope-Lifting Model

- Scope is now treated as an overlay on the no-freshening runtime skeleton, not
  as a second policy split.

- Operationally, every focused step does one of three things with the immediate
  scope prefix:
  - preserve `ScopedTree*` for ordinary unfinished local work
  - carry `ScopedTree*` across L0 conjunction handoff
  - reclassify `ScopedTree*` to `ScopedShell*` at shell-commit points

- The shell-commit points remain layer-local:
  - L0 final-tail completion
  - L1 `delay -> Deferred`
  - L2 disjunction reassociation/promotion/erasure

- Early versus late changes only where the active redex is found. It does not
  change the preserve/carry/reclassify story for scope prefixes.

## Locked Names

- `FreshCtx`
- `BranchCtx`
- `LateCtx`
- `wf-answer/core?`
- `relcall-lang`
- raw user-facing rule names such as `expand-relcall`, `invoke-delay`,
  `distribute-over-conj`, and `reassociate-left-result`

No rename rollback is allowed during stabilization unless the name itself
causes a correctness bug or import failure.

## Lower-Layer Analysis

- Inherent after the `FreshCtx` split:
  `search`, `runnable-search`, `runnable-root`, `ScopedTree`,
  `ScopedShell`, `FreshCtx`, `ShellCtx`, `LocalCtx`, `answers`, `cfg`,
  `BranchCtx`, and `LateCtx`.
- Why `FreshCtx` is separate:
  it is the pure `ScopedTree*` helper used by core scoped conjunction
  handoff, by the scoped delay/answer/fail phase rules, and by late answer
  continuation.
- Why empty fresh frames remain:
  the scoped semantics is refining source fresh-frame structure, not only
  non-empty lvar introduction. So `ScopedTree ()` is meaningful and should
  erase away, not be pruned by an extra administrative step.
- Why `ShellCtx` is separate:
  it is the committed shell path over `ScopedShell`, then extended by
  `Deferred` at L1 and `(answers + ...)` at L2.
- Why `BranchCtx` remains necessary:
  it isolates the exposed left-branch boundary used by the early hoist rule from
  inner local work. The witness is `((((a ∧ b) σ) <-+ (d σ)) × h c)`: early must
  stop at that exposed boundary and hoist, rather than descending into
  `((a ∧ b) σ)` first.
- Why `LateCtx` remains necessary:
  it is part of the shared helper grammar, but only late uses its extra
  descent power through `×`; early’s restriction still lives in the reducer.
  On the same witness `((((a ∧ b) σ) <-+ (d σ)) × h c)`, late may continue
  into `((a ∧ b) σ)` before the hoist boundary is discharged.
- Early vs late hoist remains an operational distinction, not an intended
  observable-answer distinction.
- Why there is no generic tree-prefix-to-shell step:
  it created real overlap with delay, disjunction, and rail policy steps.
  L0 owns only final-tail shellification; delay and disjunction own their
  layer-specific tree-prefix commitment rules.


What about the `every-disj`---that doesn't seem disjoint with the others. They should be checkboxes. We typically want to ensure that you have at *least* one delay in any path, so you get ... "X good property".

"conde, fresh, and every nevero-style delay gets one too."

Should we, could we, also preserve all the failure nodes? What does that do to the programs? Do you have to do something to find the next redex.
