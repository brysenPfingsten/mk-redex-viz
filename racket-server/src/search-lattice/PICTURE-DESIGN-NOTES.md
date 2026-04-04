# Picture Design Notes

These notes capture the current design judgment around pictures, freshening,
and the stored scope component `c`. They are intentionally more discursive than
`SEMILATTICE.md`, because they record open choices rather than settled
interface facts.

They are a supplemental note, not the primary repo-level semantics overview.

## Recommended Commit Story

The current refactor naturally splits into five commits:

1. `Split transpiler into subsystem modules`
2. `Introduce wf summary kernel and summary judgments`
3. `Move structural support/tests onto wf summaries`
4. `Move visible-tree denotation into picture/answer-node and switch app`
5. `Add picture/core properties and refresh semilattice docs`

This is preferable to one or two large commits because the seams are real:
transpiler structure, WF summary semantics, support/test migration, and visible
picture denotation each have their own failure modes and own bisect value.

## Operational Picture Versus Administration-Erased Picture

The term "extensional picture" can be misleading. A better phrase is
"administration-erased picture" or "semantic picture."

Current intended meanings:

- Operational picture:
  the tree shown for the current machine state, including administrative
  wrappers that matter to the operational story.
- Administration-erased picture:
  the same tree after forgetting wrappers that are only bookkeeping.

Concrete example:

- Machine state:
  `Bounced(FreshenedShell(... Answer ...))`
- Operational picture:
  `Bounced -> Freshened -> Answer`
- Administration-erased picture:
  `Freshened -> Answer`

So the second picture is not a vague denotational object. It is the visible
tree modulo administrative structure.

## What It Means to Make Picture Denotation Primary

"Make picture denotation the primary semantic interface" means:

- one module says what picture a machine configuration denotes
- app/tests/UI consume that module
- downstream code does not each reconstruct its own visible tree from raw
  machine syntax

That is exactly the role of `picture.rkt` in the current refactor. The app asks
for a picture of a configuration; it does not derive tree structure on its own.

## Strength of the Intended Picture-Preservation Property

The right strong property is not:

- every step preserves the administration-erased picture

because real computational steps should change the denoted picture.

The intended strong property is:

- purely administrative steps preserve the administration-erased picture
  exactly
- computational steps change it in one small local justified way

This is much stronger than "eventually the same answer stream," which is too
weak to support a clean small-step derivation story by itself.

## FreshenedTree Versus FreshenedShell

Current recommendation:

- keep them distinct internally
- collapse them in the exported visible picture unless a later use justifies
  exposing the distinction

Reason:

- `FreshenedTree` marks scope wrapped around tree-side payloads, including
  promoted payloads on the left of `+`
- `FreshenedShell` marks scope wrapped around enclosing frontier/shell
  structure

That distinction is mathematically useful in the machine derivation because it
records exactly which layer owns the scope wrapper. But the current visible
picture has both denote the same visible `Freshened` node.

Open question:

- should the exported operational picture eventually distinguish tree-freshening
  from shell-freshening, or should that distinction remain internal only?

## The Stored Scope Component `c`

Current judgment:

- in a well-formed configuration, `c` should be derivable from the surrounding
  freshening context
- the WF rules already enforce this agreement

In particular, `wf-state/at-scope?` treats the state's stored `c` as required to
match the ambient scope supplied by context.

So semantically, `c` appears redundant.

Reasons to keep it anyway:

- local reduction rules remain local
- fragment judgments remain self-contained
- the machine need not recompute ambient scope by walking outward through
  wrappers each time

So `c` behaves more like a cached environment register than like a zipper. A
zipper stores the full surrounding context. `c` stores only one projection of
that context: which fresh variables are currently in scope.

Open question:

- once redundancy is fully characterized, should `c` remain a first-class
  machine component, or should later presentations erase it and recover it from
  wrapper context?

## Property Work That Does Not Need Further Design Decisions

The following properties fit the current design without additional decisions:

- administration-erased picture WF on pure core configurations
- administration-erased picture WF through pure core traces
- zero shell-freshening in pure core/source states
- operational and administration-erased pictures agree in pure core
- zero `Bounced` in pure core configurations and traces

These properties are now implemented in `property-core.rkt`.

## Property Work That Still Depends on Design Choices

The following properties still depend on unresolved design intent:

- whether administrative steps above core must preserve the
  administration-erased picture exactly, or whether some higher-layer wrappers
  still count as semantically visible
- whether the exported operational picture should distinguish
  `FreshenedTree` from `FreshenedShell`
- whether `c` should remain explicit in the long-term machine presentation once
  derivability from context is fully established

## Current Recommendation

Until those choices are settled:

- keep tree/shell distinction internal
- keep `c` explicit
- strengthen properties around exact preservation of the
  administration-erased picture for clearly administrative steps
