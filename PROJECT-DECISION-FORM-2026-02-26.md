# Decision Form - Semantics Variants (2026-02-26)

Use this as a working sheet. Mark one option per decision (or mark `DEFER`) and note rationale.

## D1) Variant Expression Style (`DECIDE-NEXT`)
- Status: `DECIDED`
- Choose:
  - [ ] `I1` Strict per-variant syntax (each variant has only executable forms)
  - [x] `I2` Shared superset syntax + variant WF fragment + non-generation theorem
  - [ ] `DEFER`
- If `I2`, commit to proving fragment-closure/non-generation: [x] yes [ ] no
- Rationale:
  - Preserve visible inheritance and "small semantic delta" narrative in code/paper.
  - Lock semantic choices as relation variants over closely related syntax.

## D2) Baseline Family After Core (`DECIDE-NEXT`)
- Status: `DECIDED`
- Choose:
  - [x] Left-biased deterministic family first
  - [ ] Deterministic interleaving family first
  - [ ] `DEFER`
- Rationale:
  - Disjunction extension uses left-pointing node semantics first; interleaving is layered later.

## D3) Disjunction Representation (`DECIDE-NEXT`)
- Status: `DECIDED`
- Choose:
  - [x] Single-arrow base; add opposite arrow only in railroad extension
  - [ ] Dual-arrow from first disjunction layer
  - [ ] `DEFER`
- Rationale:
  - Keep baseline deterministic disjunction syntax minimal.
  - Reserve right-facing arrow syntax for railroad branch only.

## D4) Delay In DFS-Family (`DECIDE-NEXT`)
- Status: `DECIDED (for this implementation path)`
- Choose:
  - [ ] Delay-free DFS baseline
  - [x] Delayful DFS (no interleaving), possibly UI-collapsed admin steps
  - [ ] Keep both as sibling variants
  - [ ] `DEFER`
- Rationale:
  - First extension is explicitly relation-calls + delay/proceed.
  - Delay-free DFS remains possible as later sibling variant, but not on current lattice path.

## D5) Delay-Call Timing (`DECIDE-NEXT`)
- Status: `DECIDED`
- Choose:
  - [ ] Eager expansion under delay
  - [ ] Lazy expansion on resume/proceed
  - [x] Keep same syntax, compare both by relation variants
  - [ ] `DEFER`
- Rationale:
  - Build two call-timing variants over one syntax (`Rcall-eager`, `Rcall-lazy`).

## D6) Feature Composition Path (`DECIDE-NEXT`)
- Status: `DECIDED`
- Choose:
  - [x] Build `Core + Disjunction` and `Core + RelCalls` separately, then combine
  - [ ] Build directly as one combined extension
  - [ ] `DEFER`
- Rationale:
  - Locked lattice:
    - `L1 = Core + relcalls/delay/proceed`
    - `L2 = Core + left-disjunction`
    - `L3 = union(L1, L2)`
    - `L4 = L3 + right-disjunction`
  - Relations:
    - `Rbase-e = union(Rcall-eager, Rdisj-left)`
    - `Rbase-l = union(Rcall-lazy, Rdisj-left)`
    - `Rflip-{e,l}` extend base on left-only syntax
    - `Rrail-{e,l}` extend base on right-arrow syntax

## D7) Answer Placement
- Status: `OPEN`
- Choose:
  - [ ] External `ans*` list
  - [ ] In-tree answers
  - [ ] In-tree answers + hidden marker nodes
  - [ ] `DEFER`
- Rationale:
  - Decision should be made together with D8 (fresh markers).
  - D10 is now explicitly deferred; keep D7 scoped to current theorem/testing work.
  - Current implementation remains `external ans*` while this is unresolved.
  - Use this criterion:
    - if paper claim priority is stronger locality/provenance theorems, favor `in-tree` (`G2/G3`);
    - if priority is minimizing semantic churn this cycle, keep `external ans*` (`G1`).

## D8) Fresh-History Markers
- Status: `OPEN (DEFERRED FOR NOW)`
- Choose:
  - [ ] Keep explicit "fresh happened here" markers post-step
  - [ ] Do not keep markers
  - [x] `DEFER`
- Rationale:
  - Defer until provenance/locality theorem or UI trace requirements make marker nodes necessary.
  - Current path keeps subset-`c` precision without marker nodes in the core.

## D9) `c` Discipline For Paper-Primary Metatheory
- Status: `DECIDED`
- Choose:
  - [x] Subset-`c` primary (global-`c` as simplification)
  - [ ] Global-`c` primary
  - [ ] `DEFER`
- Rationale:
  - Committed on 2026-02-27.
  - Aligns with current implementation direction (`(s × g c)` + subset-aware WF judgments) and intended stronger locality/scoping claims.

## D10) Paper Comparison Claim (delayful vs delay-free DFS)
- Status: `DEFERRED (future work; not active now)`
- Choose:
  - [ ] Add administrative-step correspondence theorem
  - [ ] No theorem; only empirical comparison
  - [x] `DEFER`
- Rationale:
  - Keep this out of the current implementation/theorem batch.
  - Revisit after D7/D8 are settled and the primary theorem surface is stabilized.

## D11) Interleaving Semantics Family
- Status: `DEFERRED (future work; not active now)`
- Choose:
  - [ ] Keep only the currently implemented interleaving branches (`flip` and `railroad`)
  - [ ] Add Dmitri-style deterministic interleaving that rotates at every disjunction node
  - [x] `DEFER`
- Rationale:
  - This is a distinct semantics axis (scheduler policy), not just syntax.
  - It should be represented as its own relation variant so comparisons are explicit.
  - Current operational choice: hide `dmitry` from active model dispatch until the extension is implemented cleanly.
  - Planning note (2026-03-05): keep this explicitly on the TODO roadmap, but do not execute it in the current batch.

## D12) Disequality Constraints
- Status: `DEFERRED (future work; not active now)`
- Choose:
  - [ ] Keep equality-only for this paper iteration
  - [ ] Add disequality constraints as an extension family
  - [x] `DEFER`
- If adding disequality, choose rollout policy:
  - [ ] Full cross-product with all existing variants
  - [ ] Phase-gated subset (core + selected branch only), then widen
- Rationale:
  - Disequality is valuable but introduces another binary axis.
  - Full matrix expansion can become combinatorial; phase-gating limits complexity while preserving comparison value.
  - Current operational choice remains equality-only in this cycle; treat disequality as explicit future work.

## D13) Frontend/Backend Variant Dispatch (`DECIDE-NEXT`)
- Status: `DECIDED`
- Choose:
  - [ ] Keep one parser and one shared example set for all selectable models
  - [x] Add model registry with explicit parser profile + example compatibility per model
  - [ ] `DEFER`
- Rationale:
  - Multiple semantics/languages require explicit dispatch to avoid invalid parser/example/model combinations.
  - This is mostly orthogonal to semantic correctness, but blocks robust JS-side UX.
  - Implemented:
    - backend model registry (`/api/get/models`) provides parser profile/target metadata.
    - backend capability analyzer (`/api/post/analyze`) computes compatibility from source AST requirements.
    - frontend enforces compatibility through warnings and Start-button gating.
    - `microKanren-dfs-nodelay` and `dmitry` are currently hidden from active model dispatch.

## D14) WF Architecture Unification
- Status: `DECIDED`
- Choose:
  - [x] Unified WF stack with extension-based layering (`wf-kernel` -> `wf-core` -> `wf-variants`)
  - [ ] Keep duplicated core/variant WF definitions
  - [ ] `DEFER`
- Rationale:
  - Reduces duplicated judgment logic.
  - Makes language-level inheritance visible in judgments, not only syntax/reductions.
  - Uses extension-style layering (`wf-kernel`/`wf-core`/`wf-variants`) with shared invariants.

## D15) Randomized Generator Unification
- Status: `DECIDED`
- Choose:
  - [x] Shared generator kernel + shared RNG/list helpers
  - [ ] Keep separate constructive generators per file
  - [ ] `DEFER`
- Rationale:
  - Removes duplicated generation/coverage plumbing across core and variant property suites.
  - Keeps per-suite thresholds/constants local, while sharing mechanics.

## D16) Legacy Judgment Path Retirement
- Status: `DECIDED`
- Choose:
  - [x] Remove active-lane dependence on legacy `closed-*` judgment path
  - [ ] Keep legacy closed-judgment path in active lanes
  - [ ] `DEFER`
- Rationale:
  - Active execution/testing should rely on canonical core/variant stack.
  - Legacy modules may remain only as explicit deprecated/archive context until fully removed.

## D17) Surface Tier Policy (UI vs Internal Models)
- Status: `DECIDED`
- Choose:
  - [x] Surface only `L3/L4` models in UI; keep `L0/L1/L2` internal
  - [ ] Surface all models
  - [ ] `DEFER`
- Rationale:
  - UI/product workflows focus on expressive, user-relevant branches.
  - Lower layers are still retained for architecture/theory inheritance and smoke checks.
  - Heavy validation is concentrated on surfaced models; lower layers keep seam/smoke gates.

## D18) Core Config Split (`work` vs `answers`)
- Status: `DECIDED`
- Choose:
  - [x] Two-slot config `(Γ s_work a_stream)` with explicit `emit` in work tree
  - [ ] Keep mixed single-tree `(Γ s)` with embedded answer-stream `+` in active work syntax
  - [ ] `DEFER`
- Rationale:
  - Enforces a structural split between executable work and accumulated answers.
  - Removes `+` from active work-tree syntax.
  - Uses `(emit σ s_work)` as the only in-work "answer now + continuation" constructor.
  - Keeps external API payload shape stable via projection (`config2 -> legacy-view-tree`) during migration.

## Milestone Gate
- Before coding next semantic layer, decisions required: `D1-D6`.
- For current theorem/proof batch, decisions required: `D7-D8` (with `D10-D12` deferred).
