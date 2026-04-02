# Property-Based Testing Audit (2026-02-26)

Scope: current `language-refactor` branch.

## Status Update (2026-02-27)
- Completed:
  - Added shared randomized-test helper module: `racket-server/tests/helpers.rkt`.
  - Added focused helper unit tests: `racket-server/tests/helpers-tests.rkt`.
  - Added deterministic (non-random) regression checks for substitution graph invariants (`triangular?`, `occurs-free?`) in `core-judgment-forms` test block.
  - Headless runner now includes helper tests via `racket-server/tests/test-all-headless.rkt`.
- Verified:
  - `raco test racket-server/src/core-judgment-forms.rkt`
  - `raco test racket-server/tests/property-core.rkt`
  - `raco test racket-server/tests/test-all-headless.rkt`
  - `raco test racket-server/tests/property-tests.rkt`

## Findings (ordered by severity)

### 1) `P0` Legacy property suite is currently non-runnable
- Evidence:
  - [property-tests.rkt](/Users/jhemann/Code/Modeling-miniKanren-in-Redex/racket-server/tests/property-tests.rkt:7) imports `../src/definitions.rkt`, `../src/judgment-forms.rkt`, `../src/reduction-relations/reduction-relations.rkt` (moved/renamed on this branch).
  - Running `raco test racket-server/tests/property-tests.rkt` fails with module-path errors.
- Impact:
  - Main property suite is effectively disabled.
- Fix:
  - Repoint imports to current `core-*` modules (or restore compatibility shims).
  - Add a CI/runner target that fails if property test files are not executable.

### 2) `P1` Property tests are not in the main aggregate runner
- Evidence:
  - [test-all.rkt](/Users/jhemann/Code/Modeling-miniKanren-in-Redex/racket-server/tests/test-all.rkt:3) does not require `property-tests.rkt`.
- Impact:
  - Even when fixed, property tests can silently drift and stop running.
- Fix:
  - Add property suite into aggregate test runner (or dedicated property runner invoked in CI).

### 3) `P1` Test runtime is fragile in headless/system environments
- Evidence:
  - [core-definitions.rkt](/Users/jhemann/Code/Modeling-miniKanren-in-Redex/racket-server/src/core-definitions.rkt:2) requires `redex/pict`, and several `raco test` invocations crash in this environment with macOS GUI service exceptions.
- Impact:
  - Property checks are hard to run reliably in automation.
- Fix:
  - Keep semantics modules GUI-free (`redex/reduction-semantics` only).
  - Move `redex/pict` imports to rendering/demo-only modules.

### 4) `P2` Generator/guard shape risks vacuous properties
- Evidence:
  - Core properties generate full `config` and then guard with wf implication:
    - [core-reduction-relations.rkt:104](/Users/jhemann/Code/Modeling-miniKanren-in-Redex/racket-server/src/reduction-relations/core-reduction-relations.rkt:104)
    - [core-reduction-relations.rkt:132](/Users/jhemann/Code/Modeling-miniKanren-in-Redex/racket-server/src/reduction-relations/core-reduction-relations.rkt:132)
    - [core-reduction-relations.rkt:142](/Users/jhemann/Code/Modeling-miniKanren-in-Redex/racket-server/src/reduction-relations/core-reduction-relations.rkt:142)
- Impact:
  - If WF terms are sparse in raw grammar generation, checks pass mostly by implication antecedent being false.
- Fix:
  - Use targeted generators for WF terms/configs (or generate by construction from smaller WF components).
  - Track and assert hit-rate of antecedent (e.g., minimum WF-sample ratio per run).

### 5) `P2` One property has likely incorrect judgment invocation style
- Evidence:
  - [property-tests.rkt:16](/Users/jhemann/Code/Modeling-miniKanren-in-Redex/racket-server/tests/property-tests.rkt:16) uses `(closed-program? (car outs))` directly instead of `judgment-holds`.
- Impact:
  - Risk of false positives/negatives or runtime failure depending on how `closed-program?` is defined.
- Fix:
  - Normalize all judgment checks to `judgment-holds` form in property code.

### 6) `P2` Important property work is explicitly unfinished
- Evidence:
  - [core-judgment-forms.rkt:416](/Users/jhemann/Code/Modeling-miniKanren-in-Redex/racket-server/src/core-judgment-forms.rkt:416) has TODO for triangular substitution property.
- Impact:
  - Known semantic invariant is not currently covered by property testing.
- Fix:
  - Implement this as a dedicated property over WF-generated substitution states.

## Recommended Remediation Plan

1. **Repair executability first**
- Fix imports and runner inclusion (`P0/P1`).
- Separate GUI imports from semantic/test modules (`P1`).

2. **Add a property harness with metrics**
- For each property, record:
  - number of generated samples,
  - number satisfying precondition,
  - precondition hit-rate.
- Fail run if hit-rate falls below threshold.

3. **Move from raw-grammar random to WF-constructive generation**
- Create helper generators:
  - WF terms,
  - WF states,
  - WF trees,
  - WF configs.
- Keep a small number of raw grammar fuzz tests, but do not rely on them for preservation/progress confidence.

4. **Expand property inventory**
- Keep existing checks:
  - unique decomposition,
  - progress,
  - one-step WF preservation.
- Add:
  - N-step WF preservation (bounded),
  - substitution/occurs/triangular invariants,
  - fragment non-generation (if shared-syntax strategy is used).

5. **Regression discipline**
- Any found counterexample becomes a pinned regression test term.
- Keep a compact corpus file and run it in every test pass.

## Suggested Immediate Next Actions
- Create `tests/property-core.rkt` targeting only `core-*`.
- Add a tiny wrapper that prints precondition hit-rates for each property.
- Implement and enable the missing triangular-substitution property.

## Current Execution Split
- Automated/headless path:
  - run: `raco test racket-server/tests/test-all-headless.rkt`
  - currently includes:
    - core module `module+ test` checks (`core-definitions`, `core-judgment-forms`, `core-reduction-relations`),
    - core property-based checks with constructive generators and explicit coverage thresholds.
- GUI/manual path:
  - `racket-server/tests/test-all.rkt` remains the manual GUI-oriented runner.
  - it is still blocked by the broader ongoing module-path refactor in non-core app/tests files.

## Tuning Profiles (Edit Constants In-File)
- `racket-server/src/core-judgment-forms.rkt` (`module+ test`):
  - edit: `JUDGMENT-PROP-ATTEMPTS`, `JUDGMENT-PROP-SIZE`, `JUDGMENT-PROP-SEED`, `JUDGMENT-U-POOL-SIZE`, `JUDGMENT-C-MAX`
  - run: `raco test racket-server/src/core-judgment-forms.rkt`
- `racket-server/tests/property-core.rkt`:
  - edit: `PROPERTY-ATTEMPTS`, `PROPERTY-TERM-SIZE`, `PROPERTY-SEED`, `PROPERTY-U-POOL-SIZE`, `PROPERTY-X-POOL-SIZE`, `PROPERTY-R-POOL-SIZE`, `PROPERTY-C-MAX`, `PROPERTY-C-EXTRA-MAX`
  - run: `raco test racket-server/tests/test-all-headless.rkt`
