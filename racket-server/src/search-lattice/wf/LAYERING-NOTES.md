# WF Layering Notes

This note describes what a more explicitly layered `wf-*` stack would mean for
this repo, and where the current stack is and is not already layered.

## Current State

The current `wf-*` stack is layered in two limited ways:

- shared kernel helpers live in:
  - `kernel-base.rkt`
  - `summary-kernel.rkt`
  - `core-wf.rkt`
- some selected extension points already extend lower layers directly:
  - `wf-summary-goal/*`
  - `wf-summary-promoted/*`

The stack is not layered in the same strong sense as the reducer lattice for:

- `wf-summary-resolved/*`
- `wf-summary-work/*`
- `wf-summary-search/*`
- `wf-summary-frontier/*`
- `wf-summary-cfg/*`

Across `delay-wf`, `disj-wf`, `search-base-wf`, `rail-wf`, and the calls-bearing
analogs, those judgments are mostly restated at each layer with the new syntax
cases baked in.

So the current stack is:

- layered at the kernel/helper level
- selectively layered at a few judgment extension points
- mostly "reauthor per layer" for the summary-family spine

## What "Explicitly Layered" Would Mean

A more explicitly layered `wf-*` stack would make the layer boundaries visible
in the same way the reducer stack now does.

That would mean:

- define reusable lower-layer summary judgments as named base extension points,
  not just goal/promoted
- give the search-base join a real `wf` join stage, analogous to
  `search-base-pre-red`
- make later layers add only their actual delta cases instead of restating an
  entire family with copied inherited clauses

The intended layering target is:

1. kernel helpers and summary arithmetic
2. `core-wf` base summary judgments
3. `delay-wf` delta over core
4. `disj-wf` delta over core
5. `search-base-wf` join of delay-side and disjunction-side summary pieces
6. `rail-wf` delta over search-base
7. calls-bearing `wf` layers as overlays over the corresponding call-free layer

## Why This Is Not Free

- Redex judgment extension is less ergonomic than reducer extension.
- `search-base` is a genuine two-parent join, so a cleaner layering story will
  likely introduce more named judgments, not fewer.
- A too-aggressive factoring could produce a prettier diagram and a muddier
  implementation.

So the right test is not "can we remove repeated clauses?" It is:

- do the new base judgment names correspond to real semantic layer seams?
- does the resulting code make inherited cases and layer deltas easier to see?

## Recommended Pilot

Start with `wf-summary-frontier/*` and its immediate dependencies.

Why this slice first:

- it is the clearest place where delay, disjunction, search-base, and rail
  visibly diverge
- it already mirrors the shell/frontier runtime story reasonably well
- it should make the join-vs-extension issue in `search-base-wf` concrete

Pilot scope:

- identify a reusable lower-layer `frontier` base judgment family
- factor the delay-only delta
- factor the disjunction-only delta
- sketch what a `search-base` join judgment would look like
- sketch what a `rail` delta over that join would look like

Do not propagate the pattern through the rest of the stack until that pilot
either reads better or clearly fails to pay for itself.
