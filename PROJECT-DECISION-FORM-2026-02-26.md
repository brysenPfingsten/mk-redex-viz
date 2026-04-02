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
- Status: `OPEN`
- Choose:
  - [ ] Add administrative-step correspondence theorem
  - [ ] No theorem; only empirical comparison
  - [ ] `DEFER`
- Rationale:

## Milestone Gate
- Before coding next semantic layer, decisions required: `D1-D6`.
- Before theorem/proof write-up, decisions required: `D7-D10`.
