# Project Status (language-refactor) - 2026-02-26

## 0) Purpose
This note is a restart map: what has been decided, what is provisional, and what choices are still open for the semantics/paper plan.

## 0.1) Modeling Principles (explicitly adopted)
- Prefer **small, visible deltas** between variants (syntax, reduction, and judgments) so behavioral inheritance is obvious in code and paper.
- It is acceptable to use a **shared/superset syntax** when that simplifies comparison, as long as WF/progress/preservation claims are scoped to the intended fragment.
- Where possible, keep syntax fixed and vary only reduction relation to demonstrate semantic choices (eager vs lazy, DFS vs interleaving, etc.).

## 1) Current Snapshot
- Branch: `language-refactor`, synced with `origin/language-refactor` (2026-03-05).
- Semantic work has been split into `core-*` files:
  - `racket-server/src/core-definitions.rkt`
  - `racket-server/src/core-judgment-forms.rkt`
  - `racket-server/src/reduction-relations/core-reduction-relations.rkt`
- Core currently models conjunction/equality/fresh/succeed + answer collection.
- Disjunction, relation calls, delays, and interleaving are intentionally not yet in this core.
- There is a parallel legacy model (`dfs.rkt`, `dmitry-and-dmitry.rkt`) that still reflects the older architecture.
- Project intent is now explicitly a **family of semantics/languages**, including both:
  - non-railroad variants, and
  - railroad variants.
- Important clarification: once disjunction tree nodes exist in syntax, semantics must include a concrete scheduling/stepping choice for those nodes (deterministic policy is acceptable; "syntax only" is not enough for progress/preservation claims).
- Active surfaced model lattice now includes:
  - `mk-l3-{dfs,flip}-{lazy,eager}`
  - `mk-l4-rail-{lazy,eager}`
- Internal (hidden) architecture-preserving tiers remain:
  - `mk-l0-core`
  - `mk-l1-call-{lazy,eager}`
  - `mk-l2-disj-left`
- WF/generator refactor direction is now locked:
  - WF stack unification into `wf-kernel` -> `wf-core` -> `wf-variants`.
  - Shared randomized generator mechanics in dedicated kernel/support modules.
  - Active-lane retirement of legacy `closed-*` judgment dependencies.

### 1.1) Testing Hardening Progress (completed)
- Shared helper extraction completed for randomized test mechanics:
  - `racket-server/tests/helpers.rkt`
- Headless testing path includes:
  - focused helper unit tests (`helpers-tests.rkt`)
  - constructive property suite (`property-core.rkt`)
- Deterministic non-random regression checks added for substitution-graph invariants (`triangular?`, `occurs-free?`) in `core-judgment-forms`.
- Current headless execution command remains:
  - `raco test racket-server/tests/test-all-headless.rkt`

### 1.2) Legacy Test Migration Status (2026-03-05)
- Fully squeezed and removed (`git rm`):
  - `test-reduction-relations.rkt`
  - `unit-tests.rkt`
  - `translator-tests.rkt`
  - `visual-tests.rkt`
- Squeezed coverage is now in active suites:
  - canonical/transpiler checks: `racket-server/tests/test-transpiler.rkt`
  - relation/rule lifecycle checks: `racket-server/tests/variant-module-tests.rkt`
  - legacy syntax/judgment baseline checks: `racket-server/tests/test-well-formed.rkt`
  - metafunction sanity (`walk`/`unify`): `racket-server/tests/test-metafunctions.rkt`
- Archived for explicit future extension work only:
  - `racket-server/tests/archive/legacy-deprecated/test-dmitry-and-dmitry.rkt`

### 1.3) Compatibility Dispatch + Gating (implemented)
- Backend model registry is now source-of-truth for selectable semantics:
  - `GET /api/get/models` via `racket-server/src/model-registry.rkt`
- Surface policy is now split from semantics:
  - `racket-server/src/model-surface-policy.rkt` defines surfaced/heavy/internal tiers.
- Backend capability analysis is implemented:
  - `racket-server/src/capability-analysis.rkt`
  - `POST /api/post/analyze` in `racket-server/src/app.rkt`
- `init!` now enforces model/program compatibility before execution.
- Frontend behavior:
  - debounced source analysis,
  - explicit compatibility warning panel,
  - Start-button gating for syntax-error/incompatible cases.
  - note: compatibility badges in dropdown options were intentionally removed; compatibility is shown through warning + Start gating instead.

### 1.4) Test-Lane Expansion (implemented)
- Active lanes are now:
  - headless aggregate: `racket-server/tests/test-all-headless.rkt`
  - app/API regression: `racket-server/tests/test-all.rkt`
  - frontend gating logic: `npm --prefix frontend test`
  - model/example API-flow matrix:
    - `racket-server/tests/model-example-matrix-tests.rkt`
- Matrix API-flow lane validates by tier:
  - heavy (`L3/L4` surfaced): full `model × example` coverage,
  - internal smoke (`L0/L1/L2` hidden): bounded seam/smoke checks.
- Both tiers validate:
  - analyze -> switch-model -> init -> step (up to 25 or termination),
  - payload shape invariants (`step`, `stepName`, JSON `program`) at each step.
- Active-lane legacy retirement status:
  - `test-all-headless` no longer depends on `judgment-parity`/`closed-*` checks.
  - `test-all` no longer pulls `test-reification` or `test-metafunctions`.
  - app/API path now uses canonical renderer module (`canonical-json.rkt`) instead of legacy renderer entry points.

### 1.5) Active Refactor Packet (in progress)
- Canonical module targets:
  - `racket-server/src/wf-kernel.rkt`
  - `racket-server/src/wf-core.rkt`
  - `racket-server/src/wf-variants.rkt`
  - `racket-server/src/canonical-json.rkt`
  - `racket-server/src/random-test-support.rkt`
  - `racket-server/tests/generator-kernel.rkt`
- Current policy:
  - no permanent compatibility shims,
  - no requirement to preserve prior internal module names/signatures,
  - preserve documented README run/test lanes.

### 1.6) Two-Slot Core Rewrite (implemented)
- Canonical internal config is now:
  - `(Γ s_work a_stream)`
- Work-tree split:
  - removed `((⊤ σ) + s)` from active work syntax,
  - added `(emit σ s_work)` as explicit work-level answer/continuation node.
- Core and variant reductions were migrated to thread `a_stream` explicitly.
- Determinism was restored structurally by:
  - treating `emit` as a scheduling barrier in shared contexts,
  - removing side-condition rule-name/step probing fences from disjunction promotion paths.
- Renderer/API compatibility:
  - external payload shape remains unchanged (`program` JSON tree string),
  - canonical renderer now projects two-slot internal configs back to the legacy view tree.

## 2) Simple Definitions (for context)
- `global c`: treat `c` as one broad "set of extant logic vars" for the whole current computation region; easier invariants, less precision.
- `subset c`: track a base `c` and require local states/goals to use supersets/subsets as appropriate; more bookkeeping, stronger locality/scoping statements.
- "railroad/interleaving" (in your usage): scheduling behavior that rotates/defers branches using delay-like structure, beyond plain left-biased DFS.

## 3) Decision Log

### Decided (implemented in code now)
1. **Use a reduced Core fragment first** (`no disj`, `no relcalls`, no delay/interleave yet).
2. **Represent `c` as a set of logic vars** (`(u_!_ ...)`), not a numeric counter.
3. **Carry `c` at conjunction tree nodes** (`(s × g c)`), so delayed/right-goal checks can refer to captured context.
4. **Adopt subset-style WF plumbing in Core**:
   - `wf-tree?` now has a `c` parameter.
   - `lvars-subset?` is defined and used in WF rules.
5. **Add unique decomposition check** (property-level check in core reduction tests).

### Decided (scope-level, not yet fully implemented)
1. **Keep both model families in scope**:
   - with railroad-style machinery,
   - without railroad-style machinery.
2. **Treat extension choices as real language/semantics variants**, not just minor rule toggles:
   - presence/absence of railroad nodes,
   - presence/absence of delay nodes,
   - eager call expansion vs delayed/proceed-based expansion.
3. **Scheduler view is fixed**:
   - scheduling is always implicit and deterministic,
   - for any fixed `(language semantics, relation environment, query)` the induced search behavior is determined,
   - different semantics induce different deterministic function spaces from queries to searches.

### Provisional (leaning chosen, but not final paper commitment)
1. **Architecture direction**: "core + strategy extensions" instead of one monolithic semantics.
2. **Subset-`c` direction** as the stronger invariant path (currently in code), while recognizing global-`c` is still a simplification fallback.
3. **Use `extend-*` style deltas to present variants** (not just as implementation convenience, but as a scientific/pedagogical presentation choice).

### Decided (paper-primary metatheory direction)
1. **`c` discipline is now locked**:
   - paper-primary: **subset-`c` precision**,
   - global-`c` may be presented only as a simplification baseline.

### Decided (variant lattice)
1. **Expression strategy**: `D1 = I2` shared/superset syntax with fragment discipline.
2. **First extension pair is locked**:
   - `L1 = Core + relation calls + delay/proceed`.
   - `L2 = Core + left-pointing disjunction nodes`.
3. **Union path is locked**:
   - `L3 = union(L1, L2)`.
   - `L4 = L3 + right-pointing disjunction nodes`.
4. **Call timing axis is locked as relation variants over same syntax**:
   - `Rcall-eager`, `Rcall-lazy`.
5. **Base and branch relations are locked**:
   - `Rbase-e = union(Rcall-eager, Rdisj-left)`
   - `Rbase-l = union(Rcall-lazy, Rdisj-left)`
   - `Rflip-e` / `Rflip-l` from left-only branch behavior
   - `Rrail-e` / `Rrail-l` from right-arrow railroad behavior

### Open / unresolved
1. **How relation-call + subset-`c` invariants are enforced/proved** per branch.
2. **Answer representation**:
   - keep external `ans*` (current core),
   - or move answer stream fully inside tree for stronger/localer structural invariants (including optional hidden marker nodes for freshening origin/scope tracking).
3. **Fresh-history marker nodes**:
   - whether to keep explicit "fresh happened here" nodes even after stepping past them, to support local subset-`c` reasoning and exposition.
   - **current stance**: deferred for now; revisit when provenance theorem/UI trace needs appear.
4. **DFS pedagogy choice**:
   - delay-free DFS model as baseline, or
   - delayful DFS model with no interleaving (possibly UI-collapsing administrative delay steps).
5. **Theorem surface**: exact claim set for progress/preservation/frame/locality across model variants.
6. **Frontend syntax/UI axis** (miniKanren + microKanren surfaces with shared backend): desired, but currently tabled.
7. **Interleaving policy family coverage**:
   - whether to add Dmitri-style deterministic interleaving (rotate at every disjunction node)
   - as a first-class relation variant, alongside current `flip` and `railroad` branches.
   - status: **future work (explicitly not in current implementation batch)**.
8. **Disequality constraints axis**:
   - whether to add disequality constraints as an extension family in this paper cycle,
   - and whether to phase-gate it to selected variants vs full lattice cross-product.
9. **JS dispatch architecture (advanced phase)**:
   - current baseline is implemented (capability analyzer + start gating + model registry),
  - open future work is multi-surface parser/profile support beyond the current canonical target path.
10. **Answer placement (D7)**:
  - still open and coupled to fresh-history marker scope.
11. **Fresh-history markers (D8)**:
  - still deferred pending theorem/provenance scope choice.

### 3.1) Legacy-to-Variant Migration Targets (working map)
- `reduction-relations.rkt` (legacy "microKanren" backend) -> **`Rrail-l`** as closest lattice target.
  - Rationale: lazy call expansion + railroad left/right delay transitions are the closest behavioral match.
- `dfs.rkt` (legacy DFS backend) -> **`Rbase-l`** as closest current lattice target.
  - Caveat: this is the closest available branch today; an exact "delay-free DFS + calls + disj" target is still a candidate sibling variant.
- `dmitry-and-dmitry.rkt` -> **no 1:1 lattice target yet**.
  - This remains an explicitly different semantics family pending transformation/embedding strategy.

## 4) Dependency Map (condensed)

High-value dependency edges (trimmed from earlier detailed map):
- `subset-c` -> requires captured conjunction context (`(s × g c)`) and branch-aware WF arguments.
- Disjunction syntax -> requires explicit deterministic scheduler semantics (cannot stay syntax-only).
- Call timing (`eager` vs `lazy`) is intentionally expressed as relation variants over shared syntax.
- `L3` composition requires combining both call-timing and disjunction stepping policies cleanly.
- Answer placement (`external ans*` vs in-tree) directly controls theorem surface and marker-node value.
- Disequality and Dmitri-style interleaving are separate axes and remain intentionally deferred.

## 5) Recommended Roadmap (short)
1. **(Completed) Freeze semantic kernel contract**:
   - paper-primary `c` discipline is locked to subset-`c` precision (global-`c` as simplification note).
2. **(Completed) Freeze semantic stratification pathway**:
   - `Core -> L1/L2 -> L3 -> L4`.
   - eager/lazy are relation variants over shared syntax.
3. **Implement the lattice modules + relation family**.
3. **State theorem targets per layer**:
   - minimum: progress + WF preservation,
   - optional stronger layer: locality/frame-style theorem (subset-c benefit).
   - include a small "administrative-step correspondence" statement when comparing delayful DFS vs delay-free DFS.
4. **Testing revamp plan (explicitly, given reviewer skepticism)**:
   - move from pure random checks to mixed strategy:
     - curated regression corpus,
     - size-controlled generators,
     - mutation/metamorphic tests,
     - model cross-checks between layers where expected.
5. **Table UI dual-surface work as a separate track**:
   - keep on roadmap, do not block semantic decisions.

## 5.1) Outstanding Decisions Tracker (explicit)
Status legend:
- `OPEN` = not committed.
- `DECIDE-NEXT` = should be decided before adding the next semantic layer.

Current priorities:
1. `OPEN`: **Answer placement (D7)**.
2. `OPEN`: **Fresh-history markers (D8)**.

Deferred (explicitly not current batch):
1. Theorem comparison claim for delayful vs delay-free DFS (D10).
2. Dmitri-style interleaving axis (D11).
3. Disequality extension axis (D12).

For full per-decision checklist and rationale, use:
- `PROJECT-DECISION-FORM-2026-02-26.md`

## 6) Testing Quality Upgrade Plan (concrete)
- Add a "property inventory" doc: each property, intended bug class, generator assumptions.
- Add targeted generators for well-formed terms/configs (not raw grammar-only random terms).
- Track distribution metrics from generators (size/depth/constructor frequency), and fail tests if distribution collapses.
- Keep and grow a regression corpus from every found counterexample.
- Add differential checks where possible:
  - core deterministic model vs equivalent older behavior on overlapping fragment.
- Keep unique decomposition/progress/preservation checks but gate them by strong WF predicates.

## 7) What Is Safe To Defer
- Full app/test import-path repair and front-end wiring.
- microKanren dual UI architecture.
- Railroad-specific implementation details.

None of these need to block settling the semantics roadmap and theorem priorities first.

## 8) Latest Audit + Fix Cycle (2026-03-05, condensed)

What we keep from the historical trail:
- Known rail-context answer-collection bug was fixed and covered by regression tests.
- Cross-product model/example matrix audit now runs as an explicit lane.
- Active surfaced models currently pass matrix + API-flow checks in bounded runs.
- Deferred variants (`dmitry`, `dfs-nodelay`) remain hidden by design.

Older detailed exploratory logs are intentionally left to git history and `Misc/audit-logs`.

## 9) Next Decision Packet: Answer Placement + Theorem Surface

This is the next meaningful design/theory blocker.

### 9.1) Answer placement options (D7)
- `G1` external `ans*` list (current):
  - simplest runtime/UI model,
  - weaker locality/provenance statements.
- `G2` in-tree answers:
  - stronger local structural invariants,
  - more tree constructors and reduction plumbing.
- `G3` in-tree answers + hidden provenance markers:
  - strongest local reasoning story (especially with subset-`c`),
  - highest complexity and proof/test overhead.

### 9.2) Fresh marker coupling (D8)
- If D7 moves toward `G2/G3`, marker nodes become much more compelling.
- If D7 stays `G1`, marker nodes are optional and can remain deferred.

### 9.3) Theorem surface coupling (current batch vs deferred)
- Immediate theorem/testing target (independent of D7 final choice):
  - tighten and document invariant claims for current lattice:
    - WF preservation,
    - progress (fragment-scoped),
    - deterministic one-step decomposition in intended fragments.
- Deferred:
  - D10-style delayful-vs-delay-free correspondence claims are explicitly out of the current batch.
- If D7 picks `G2/G3`, add locality/provenance theorem candidates:
  - branch-local variable-origin alignment,
  - no-cross-branch contamination beyond shared-prefix `c`.

### 9.4) Recommended near-term sequence
1. Write a short "property inventory" doc mapping each claim to:
   - bug class prevented,
   - current test coverage,
   - missing generator/lemma support.
2. Decide D7 explicitly (pick one of `G1/G2/G3`).
3. Resolve D8 based on D7 choice.

### 9.5) D7 decision rubric (maximal WF/theorem leverage)

Use this to choose answer placement with explicit theorem/testing tradeoffs.

- `G1` external `ans*` (current):
  - WF/theorem upside:
    - easiest to keep existing progress/preservation checks stable.
    - minimal semantic churn and smallest proof delta.
  - WF/theorem downside:
    - weaker local structural claims about provenance/origin of answers.
    - fresh-history markers add limited value unless separately encoded.
  - Test impact:
    - mostly incremental hardening of current lanes.
    - fastest route to "more confidence now."

- `G2` in-tree answers:
  - WF/theorem upside:
    - stronger local invariants over one unified search-tree object.
    - clearer statements about branch-local evolution and answer emergence.
  - WF/theorem downside:
    - requires additional tree WF rules + preservation cases.
    - moderate rewrite of stepping/collection invariants.
  - Test impact:
    - expand matrix/property tests to tree-answer constructors.
    - medium implementation and theorem effort.

- `G3` in-tree answers + fresh-history markers:
  - WF/theorem upside (maximal):
    - strongest provenance story for subset-`c`.
    - enables marker-alignment theorems (origin/scope consistency) directly over syntax.
  - WF/theorem downside:
    - highest semantic complexity and proof burden.
    - risk of slowing near-term stabilization.
  - Test impact:
    - requires new marker-specific invariants and regression suites.
    - largest implementation/theory surface.

Recommendation for current phase:
- If goal is immediate theorem-test confidence with low churn: choose `G1`.
- If goal is stronger subset-`c` provenance story this cycle: choose `G2` now, stage `G3` later.
