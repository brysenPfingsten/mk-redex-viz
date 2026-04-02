# Project Status (language-refactor) - 2026-02-26

## 0) Purpose
This note is a restart map: what has been decided, what is provisional, and what choices are still open for the semantics/paper plan.

## 0.1) Modeling Principles (explicitly adopted)
- Prefer **small, visible deltas** between variants (syntax, reduction, and judgments) so behavioral inheritance is obvious in code and paper.
- It is acceptable to use a **shared/superset syntax** when that simplifies comparison, as long as WF/progress/preservation claims are scoped to the intended fragment.
- Where possible, keep syntax fixed and vary only reduction relation to demonstrate semantic choices (eager vs lazy, DFS vs interleaving, etc.).

## 1) Current Snapshot
- Branch: `language-refactor`, ahead of `origin/language-refactor` by 4 commits.
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

### 1.1) Testing Hardening Progress (completed)
- Shared helper extraction completed for randomized test mechanics:
  - `racket-server/tests/helpers.rkt`
- Headless testing path includes:
  - focused helper unit tests (`helpers-tests.rkt`)
  - constructive property suite (`property-core.rkt`)
- Deterministic non-random regression checks added for substitution-graph invariants (`triangular?`, `occurs-free?`) in `core-judgment-forms`.
- Current headless execution command remains:
  - `raco test racket-server/tests/test-all-headless.rkt`

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
8. **Disequality constraints axis**:
   - whether to add disequality constraints as an extension family in this paper cycle,
   - and whether to phase-gate it to selected variants vs full lattice cross-product.
9. **JS dispatch architecture**:
   - parser profile may differ by selected language/semantics,
   - example dropdown should be model-compatible rather than global.

### 3.1) Legacy-to-Variant Migration Targets (working map)
- `reduction-relations.rkt` (legacy "microKanren" backend) -> **`Rrail-l`** as closest lattice target.
  - Rationale: lazy call expansion + railroad left/right delay transitions are the closest behavioral match.
- `dfs.rkt` (legacy DFS backend) -> **`Rbase-l`** as closest current lattice target.
  - Caveat: this is the closest available branch today; an exact "delay-free DFS + calls + disj" target is still a candidate sibling variant.
- `dmitry-and-dmitry.rkt` -> **no 1:1 lattice target yet**.
  - This remains an explicitly different semantics family pending transformation/embedding strategy.

## 4) Dependency Map (what constrains what)

### A. `c` discipline
- `A1 = global-c`
- `A2 = subset-c` (current core implementation)

Implications:
- If `A1`: simpler WF/proofs/tests; weaker locality/frame statements.
- If `A2`: more syntax/judgment plumbing; enables stronger branch-local and scoping claims.

### B. Conjunction representation
- `B1 = plain (s × g)` (no captured c)
- `B2 = (s × g c)` (current core implementation)

Dependencies:
- If `A2` then `B2` is effectively required.
- If `A1` either `B1` or `B2` can work.

### C. Search strategy layering
- `C1 = deterministic DFS-style base`
- `C2 = deterministic + fair/interleaving extension`
- `C3 = railroad extension`

Dependencies:
- `C3` depends on decisions about delay and disjunction orientation.
- Keeping `C1` clean first reduces proof/test complexity.

### D. Disjunction design
- `D1 = single disjunction form in base`
- `D2 = dual orientation / railroad-specific forms`
- `D3 = chosen deterministic scheduler semantics for disjunction nodes`

Dependencies:
- If disjunction nodes are in syntax, `D3` is required to keep progress/preservation statements meaningful.
- `C3` likely requires `D2` (or an equivalent explicit scheduler mechanism).
- `C1` is easiest with `D1`.

### E. Relation-call expansion
- `E1 = direct call expansion` (DFS-friendly)
- `E2 = call introduces delay/proceed` (railroad/interleave-friendly)

Dependencies:
- `E2` interacts strongly with `C2/C3` and delay semantics.
- `A2` requires explicit WF argument that substitution/call expansion does not violate captured `c` invariants.

### E2. Delay + call timing policy
- `E2a = eager (expand under delay immediately)`
- `E2b = lazy (expand only when resumed/proceed is selected)`
- `E2c = unified syntax, alternate reduction relations (same AST, different stepping discipline)`

Dependencies:
- `E2b` usually requires suspended-call syntax (`proceed`/thunk-like node).
- `E2a` can often avoid extra suspension syntax but may do extra administrative work.
- `E2c` is the preferred comparison style when the paper goal is "same language, different operational interpretation."

### F. Feature-composition pathway
- `F1 = Core + Disjunction (no relcalls/recursion)`
- `F2 = Core + RelCalls/recursion (no disjunction)`
- `F3 = Combined (Disjunction + RelCalls/recursion)`

Dependencies:
- `F3` requires conflict resolution between disjunction scheduling (`D*`) and call expansion policy (`E*`).
- This split reduces design risk by validating each extension independently before composition.

### J. Constraint-store expressivity
- `J1 = equality-only`
- `J2 = equality + disequality`

Dependencies:
- `J2` adds a new semantic/testing/proof axis (constraint-store behavior and WF invariants).
- If combined with every scheduler/call-timing branch, matrix size grows quickly (cross-product effect).
- Recommended containment: stage `J2` on a selected baseline branch first, then widen only if needed.

### G. Answer-stream placement
- `G1 = external ans* list` (current core)
- `G2 = answers kept inside tree`
- `G3 = in-tree answers + hidden marker/scope nodes (e.g., freshening-origin markers)`

Dependencies:
- `G1` is simpler operationally and matches current implementation.
- `G2/G3` may support stronger local structural theorems (scope/origin tracking without recomputation).

### I. Variant expression style
- `I1 = strict per-variant syntax (only forms that execute in that variant)`
- `I2 = shared superset syntax + per-variant WF fragment`

Dependencies:
- `I2` can make extension deltas cleaner and side-by-side comparisons clearer.
- `I1` keeps each variant semantically minimal but can increase duplication and weaken visible inheritance story.
- If using `I2` while some constructors are not handled by a given variant relation, you need an explicit **fragment-closure / non-generation** result:
  - starting from a variant-well-formed term, excluded constructors are never generated,
  - therefore progress/preservation are proved over that variant fragment rather than over all raw syntax.

### H. UI syntax support (orthogonal)
- `H1 = miniKanren only`
- `H2 = miniKanren + microKanren frontends, shared backend`

Dependencies:
- Mostly parser/transpiler/UI work, low coupling to core semantic correctness, but affects test surface.

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
- Status legend:
  - `OPEN` = not committed.
  - `DECIDE-NEXT` = should be decided before adding the next semantic layer.

1. `DECIDED`: **Variant expression style**
   - `I2`: shared/superset syntax + fragment discipline.
2. `DECIDED`: **Baseline family after Core**
   - deterministic left-biased branch first.
3. `DECIDED`: **Disjunction representation**
   - single-arrow base first; right-arrow only in railroad extension.
4. `DECIDED (current path)`: **Delay in DFS-family variants**
   - delayful path first (`L1`); delay-free DFS remains optional later sibling.
5. `DECIDED`: **Delay-call timing (same syntax, different relations)**
   - eager and lazy both first-class.
6. `OPEN`: **Answer placement**
   - external `ans*`,
   - in-tree answers (+ optional hidden marker nodes).
7. `OPEN`: **Fresh-history markers**
   - keep explicit "fresh happened here" markers after step,
   - do not keep markers.
   - current handling: deferred as an extension issue (not a blocker for core path).
8. `DECIDED`: **`c` discipline for paper-primary metatheory**
   - **subset-`c` primary** (global-`c` as simplification baseline only).
9. `OPEN`: **Interleaving policy coverage**
   - current implemented branches: `flip`, `railroad`.
   - candidate additional branch: Dmitri-style "interleave at every disjunction node."
10. `OPEN`: **Disequality constraints**
   - add as extension family or defer.
   - if added, choose full-lattice rollout vs phased rollout (recommended).
11. `OPEN`: **Frontend/backend variant dispatch**
   - one shared parser/example set vs model registry with parser+example compatibility.
   - currently: partial guardrails on frontend example filtering; full parser-profile dispatch still open.

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
