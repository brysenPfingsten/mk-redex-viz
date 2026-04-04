# L0-L3 Semilattice And Context Overlay

This note separates two different structures:

- the runtime language semilattice
- the context/decomposition overlay used by the reducers

Those are related, but they are not the same thing.

## Runtime Lattice

```mermaid
graph TD
  coreNode["L0 core"]
  delayNode["L1 delay"]
  disjNode["L2 disj"]
  disjEarlyNode["L2 disj early"]
  disjLateNode["L2 disj late"]
  searchNode["L3 search"]
  searchEarlyNode["L3 search early"]
  searchLateNode["L3 search late"]

  coreNode --> delayNode
  coreNode --> disjNode
  delayNode --> searchNode
  disjNode --> searchNode
  disjNode --> disjEarlyNode
  disjNode --> disjLateNode
  searchNode --> searchEarlyNode
  searchNode --> searchLateNode
  disjEarlyNode --> searchEarlyNode
  disjLateNode --> searchLateNode
```

Main meet/join claims:

- `glb(delay, disj) = core`
- `lub(delay, disj) = search`
- `lub(delay, disj-early) = search-early`
- `lub(delay, disj-late) = search-late`

`rail`, `relcall`, and strategy layers are follow-on extensions. They are not part
of this primary L0-L3 lattice.

## No-Freshening Core Model

Before layering variable-scope bookkeeping back in, the lower lattice has one
clean shared runtime story:

```text
L0/core
tail0 ::= answer
        | empty
        | (g σ)
        | (tail0 × g)

L1/delay extends L0
cfg1  ::= tail1
        | Deferred cfg1

tail1 ::= tail0
        | delay(work)

L2 neutral disj extends L0
cfg2  ::= tail2
        | answer + cfg2

tail2 ::= tail0
        | (tail2 <-+ tail2)

answer ::= answer

L3/search = join(L1, L2 neutral disj)
cfg3  ::= tail3
        | Deferred cfg3
        | answer + cfg3

tail3 ::= tail0
        | delay(work)
        | (tail3 <-+ tail3)
```

At this level:

- `Deferred` is committed delay shell
- `+` is committed answer shell
- the remaining `tail` is the one active-or-final tail under that shell

The key point is that `L2 neutral disj` extends `L0`, not `L1`. The `L3`
search layer is the runtime join of delay and neutral disjunction.

## No-Freshening Context Model

Ignoring freshening, the core local path already belongs to `L0`:

```text
Local ::= hole
        | (Local × g)
```

This is the path to the currently active local core work.

For late hoist, the active path needs to follow both conjunction and leftward
disjunction:

```text
Late ::= hole
       | (Late × g)
       | (Late <-+ tail)
```

After the freshening layer is restored, the shared context grammar carries both
the smaller left-branch helper and the larger late-strength helper. Early and
late then diverge in their reducers, not by using separate context languages.

For early/eager hoist, we intentionally keep the same underlying runtime grammar
and make the policy difference live in the contexts and reduction relation, not
in an early-specific runtime constructor.

## Scope Overlay

Once the no-freshening story is fixed, scope is best viewed as a lifting layer
over that bare skeleton, not as a second semantic redesign.

The runtime split is:

```text
terminal-search ::= answer
                  | empty

runnable-root ::= (g σ)
                | (search × g)

search ::= terminal-search
         | runnable-root
         | delay(runnable-search)
         | (search <-+ search)
         | ScopedTree(c, search)

runnable-search ::= runnable-root
                  | ScopedTree(c, runnable-search)

cfg ::= search
      | ScopedShell(c, cfg)
      | Deferred cfg
      | answers + cfg

answers ::= answer
           | ScopedTree(c, answers)
```

Operationally, a reduction focuses on the same no-freshening skeleton as
before. The only extra question is what to do with the maximal immediate scope
prefix attached to the focused subterm.

There are only three cases:

1. Preserve
   - ordinary local work keeps the same `ScopedTree*` prefix
   - example:
     `ScopedTree*(g σ) -> ScopedTree*(...)`

2. Carry
   - L0 conjunction handoff moves the innermost `ScopedTree*` prefix from a
     resolved left result onto the right-hand continuation
   - example:
     `(ScopedTree*(⊤ σ) × g) -> ScopedTree*(g σ)`

3. Reclassify
   - crossing from unfinished tree into committed shell converts the
     enclosing frontier prefix into `ScopedShell*`
   - examples:
     - final tail:
       `ScopedTree* atom -> ScopedShell* atom`
     - delay commit:
       `ScopedTree*(delay work) -> ScopedShell*(Deferred work)`
     - disjunction promotion:
       outer frontier scope shellifies, but the answers payload stays
       `ScopedTree* answer` on the left of `+`

This is why two freshening roles are enough:

- `ScopedTree` marks scope attached to tree-side payloads, including
  answers payloads on the left of `+`
- `ScopedShell` marks scope attached to the enclosing shell/frontier
  structure

The important correction is that every recursive search child position is
itself a full `search` position. So freshened tree prefixes can reappear at
each subtree boundary, not just at the root of the whole search.

For example, a shape like

```text
ScopedTree(c0,
  (ScopedTree(c1,
     (ScopedTree(c2, (a σ)) <-+ (b σ)))
   × h))
```

is exactly the intended kind of nested scoped search:

- `c0` scopes the whole conjunction subtree
- `c1` scopes the left conjunct subtree
- `c2` scopes the left branch inside the disjunction

So the right mental model is not "one prefix on the whole tree". It is
"every search node may be preceded by a finite `ScopedTree*` prefix".

We do not need a third freshening role for `answers`. The constructor
`answers` already carries the "committed answer payload" distinction.

### Representative Lifted Traces

L0 scoped conjunction handoff:

```text
(ScopedTree(c1, ⊤σ) × g)
-> ScopedTree(c1, gσ)
```

L1 delay commitment:

```text
ScopedTree(c1, delay(work))
-> ScopedShell(c1, Deferred(work))
```

L2 answer promotion:

```text
ScopedTree(c1, ⊤σ) <-+ right
-> ScopedTree(c1, ⊤σ) + right
```

The policy split does not change those prefix actions. Early and late differ
only in where they focus inside the unfinished tail, not in how
`ScopedTree*` and `ScopedShell*` move once the focal redex is chosen.

### Scoped Early Versus Late Witnesses

Early/eager hoist on an exposed boundary preserves the left branch's
`ScopedTree*` prefix but hoists immediately:

```text
((ScopedTree(c1, (a σ)) <-+ (b σ)) × h)
-> ((ScopedTree(c1, (a σ)) × h) <-+ ((b σ) × h))
```

Late hoist keeps descending into the left branch under that same exposed
boundary, still preserving the left branch's `ScopedTree*` prefix:

```text
((ScopedTree(c1, ((a1 ∧ a2) σ)) <-+ (b σ)) × h)
-> ((ScopedTree(c1, ((a1 σ) × a2)) <-+ (b σ)) × h)
```

Then, once the left branch resolves, late uses the same L0 carry action as
before:

```text
((ScopedTree(c1, ⊤σ1) <-+ (b σ)) × h)
-> (ScopedTree(c1, hσ1) <-+ ((b σ) × h))
```

So the scoped late-only witness is:

```text
((ScopedTree(c1, ((a1 σ) × a2)) <-+ (b σ)) × h)
```

Late may reach that shape. Early may not, because early must hoist as soon as the
outer `((alpha <-+ beta) × gamma)` boundary is exposed.

### Full Scoped Source-To-Runtime Traces

Take one scoped source program:

```text
((fresh(x, a1 ∧ a2) ∨ b) ∧ h) σ0
```

The common prefix of the early and late traces is:

```text
((fresh(x, a1 ∧ a2) ∨ b) ∧ h) σ0
-> (((fresh(x, a1 ∧ a2) ∨ b) σ0) × h)
-> (((fresh(x, a1 ∧ a2) σ0) <-+ (b σ0)) × h)
-> ((ScopedTree(c1, ((a1 ∧ a2) σ1)) <-+ (b σ0)) × h)
```

Here `c1` is the scope bundle introduced by `fresh`, and `σ1` is the state
after substitution.

Early diverges immediately at the exposed branch/conjunction boundary:

```text
((ScopedTree(c1, ((a1 ∧ a2) σ1)) <-+ (b σ0)) × h)
-> ((ScopedTree(c1, ((a1 ∧ a2) σ1)) × h) <-+ ((b σ0) × h))
-> ((ScopedTree(c1, ((a1 σ1) × a2)) × h) <-+ ((b σ0) × h))
```

So early hoists first, and only then continues local work under the preserved
`ScopedTree(c1, ...)` prefix.

Late diverges by descending into the left branch before hoisting:

```text
((ScopedTree(c1, ((a1 ∧ a2) σ1)) <-+ (b σ0)) × h)
-> ((ScopedTree(c1, ((a1 σ1) × a2)) <-+ (b σ0)) × h)
```

If the left branch continues to success, late eventually reuses the same L0
carry rule under that preserved scope prefix:

```text
((ScopedTree(c1, (⊤σ2)) <-+ (b σ0)) × h)
-> (ScopedTree(c1, hσ2) <-+ ((b σ0) × h))
```

So the policy split is:

- early changes the tree shape first, then keeps stepping under the same tree
  prefix
- late keeps the tree shape longer, steps under the same tree prefix, and
  only later continues the surrounding conjunction

## Context Overlay

```mermaid
graph TD
  qfresh["FreshCtx (pure tree-fresh prefix)"]
  qshell_delay["ShellCtx(delay)"]
  qshell_disj["ShellCtx(disj)"]
  qshell_search["ShellCtx(search)"]

  klocal["LocalCtx (L0 local work path)"]
  kbranch["BranchCtx (shared L2 left-branch path)"]
  klate["LateCtx (late-hoist extension)"]

  qfresh --> qshell_delay
  qfresh --> qshell_disj
  qshell_delay --> qshell_search
  qshell_disj --> qshell_search

  klocal --> kbranch
  kbranch --> klate
```

This is a reuse graph inside the shared context grammar, not a second
semilattice.

- machine criterion:
  prefer the decomposition that still looks like a clean pre-image for
  refocusing + fusion, so the runtime grammar stays broad and the scoped phase
  story is expressed through `FreshCtx` rather than through per-node scoped
  runtime families
- `FreshCtx` is the pure `ScopedTree*` helper used for scoped handoff.
- `FreshCtx` is also the locked Option D helper for phase-boundary heads only:
  `(delay runnable-search)`, `(⊤ σ)`, and `(empty-tree)`.
- empty fresh-frame note:
  `ScopedTree ()` and `ScopedShell ()` are now real frames, not garbage to
  prune. The scoped machine keeps source fresh-frame nesting even when the
  intro list is empty, and erase-scope drops those frames without needing a
  separate stuttering prune step.
- `ShellCtx(delay)` is the committed shell path for `ScopedShell` and
  `Deferred`.
- `ShellCtx(disj)` is the committed shell path for `ScopedShell` and
  `(answers + ...)`.
- `ShellCtx(search)` is the union of those shell stories.
- `LocalCtx` is the L0 local-work path: a pure `FreshCtx` bottom or one more
  pending conjunction layer around a smaller `LocalCtx`.
- `BranchCtx` is the shared L2 left-branch path through `<-+`.
- `LateCtx` is the shared late-strength helper that keeps descending past the early
  cut through `×`; early does not use that extra descent rule, but late does.

Witness for why `BranchCtx` and `LateCtx` both remain necessary:

```text
((((a ∧ b) σ) <-+ (d σ)) × h c)
```

- early stops here and hoists:
  `((((a ∧ b) σ) × h c) <-+ ((d σ) × h c))`
- late keeps descending into `((a ∧ b) σ)` first

That is an operational distinction, not an intended observable-answer
distinction.

Reuse rule:

- reuse an earlier helper in two directions only when the earlier layer does
  not depend on the later meanings
- once a helper acquires a new stopping or decomposition role, introduce that
  helper at the first divergent layer instead of predeclaring it below

## Node Inventory

### L0 core

| Aspect | Value |
| --- | --- |
| Runtime constructors added | `search`, `cfg`, `answer`, `empty-tree`, `(search × g c)`, `ScopedTree`, `ScopedShell`, core goals |
| Helpers introduced or extended | `FreshCtx`, `ConjCtx`, `LocalCtx`, `ShellCtx` |
| First reducer family using them | `core-red` |

Important L0 boundary:

- `ScopedTree` marks tree-side payload scope, even when a answers answer
  has already moved onto the left of `+`
- `ScopedShell` marks enclosing committed shell/frontier scope
- L0 owns only the final tree-to-shell lift for terminal tails

### L1 delay

| Aspect | Value |
| --- | --- |
| Runtime constructors added | `suspend` goal, `delay`, `Deferred` |
| Helpers introduced or extended | `ShellCtx(delay)` |
| First reducer family using them | `delay-red` |

Delay-specific shell commitment:

- `invoke-delay` is the layer-specific rule that can take a pure
  `ScopedTree*` prefix around `delay` and commit it into `ScopedShell*`
  around `Deferred`

### L2 neutral disj

| Aspect | Value |
| --- | --- |
| Runtime constructors added | `∨` goal, `<-+`, `answers`, `+` |
| Helpers introduced or extended | `ShellCtx(disj)`, `BranchCtx`, `LateCtx` |
| First reducer family using them | `disj-base-red` |

Neutral disjunction commitment:

- answers keep `ScopedTree*` payloads on the left of `+`
- disjunction frontier rules can still commit the enclosing frontier prefix
  into shell at the point where an answer is reassociated, promoted, or erased

### L2 early

| Aspect | Value |
| --- | --- |
| Runtime constructors added | none; this is a policy/context refinement of neutral disjunction |
| Helpers introduced or extended | none; early uses the shared context grammar |
| First reducer family using them | `disj-early-red` |

Early policy:

- decomposition is `ShellCtx ∘ BranchCtx ∘ LocalCtx`
- early stops at the branch/conjunction cut and hoists there

### L2 late

| Aspect | Value |
| --- | --- |
| Runtime constructors added | none; this is a policy/context refinement of early |
| Helpers introduced or extended | none; late uses the shared context grammar |
| First reducer family using them | `disj-late-red` |

Late policy:

- decomposition is `ShellCtx ∘ LateCtx`
- late descends past the early cut and continues or erases only once the left
  branch resolves

### L3 neutral search

| Aspect | Value |
| --- | --- |
| Runtime constructors added | none; this is `delay ∪ disj` |
| Helpers introduced or extended | `ShellCtx(search)` by language union |
| First reducer family using them | `search-local/base`, `search-shell/base` |

### L3 early

| Aspect | Value |
| --- | --- |
| Runtime constructors added | none; this is `delay ∪ disj-early` |
| Helpers introduced or extended | inherited shared `BranchCtx` / `LateCtx` grammar |
| First reducer family using them | `search-early-pre-red`, `search-early-red` |

### L3 late

| Aspect | Value |
| --- | --- |
| Runtime constructors added | none; this is `delay ∪ disj-late` |
| Helpers introduced or extended | inherited shared `BranchCtx` / `LateCtx` grammar |
| First reducer family using them | `search-late-pre-red`, `search-late-red` |

## Reading The Reducers

The intended nested-subcontext style is:

- outer committed shell
- branch/policy path
- inner local work

Concretely:

- core: `ShellCtx ∘ LocalCtx`, where `ShellCtx` is only `ScopedShell*`
- delay: `ShellCtx(delay) ∘ LocalCtx`
- disj-early: the hoist rule is exposed at `ShellCtx(disj) ∘ BranchCtx`, while local
  work stays under the same shared grammar
- disj-late: `ShellCtx(disj) ∘ LateCtx`
- search early/late: the same pattern lifted through the `delay/disj` join

Within L0 itself, `LocalCtx` is:

- a pure `FreshCtx` bottom, or
- one more pending conjunction layer around a smaller `LocalCtx`

The important semantic boundary is:

- L0 shellification is final-tail only
- delay and disjunction own their own tree-prefix-to-shell commitment rules
- shell commitment never happens below active branch/work constructors by a
  generic catch-all shell rule

## Policy Decision In The No-Freshening Model

For the no-freshening design, the current decision is:

- keep one shared search-tree runtime grammar for early and late
- keep one shared context grammar for early and late
- keep the policy difference in the reduction layer
- use incremental eager hoist for early
- use late hoist for late

This means:

- early does not get its own runtime-only pending-hoist constructor
- early does not get its own context-language split either
- some search-tree shapes are grammatical in the shared runtime language but
  unreachable under the early policy
- that is intentional; the policy difference is a reachability fact, not a
  syntax fact

Rejected alternative:

- reifying the disj/conj boundary as an explicit `HoistPending`-style runtime
  constructor
- this was rejected because it would add machinery to both policies and make
  the shared lattice less additive

The early invariant is:

- once an exposed `((alpha <-+ beta) × gamma)` appears on the active path, the
  next early step must be the hoist
- early may not make progress inside `alpha` first

The late invariant is:

- late may keep descending on the active left path through both `<-+` and `×`
- only once the left branch resolves does late continue or erase at that
  boundary

So weak/incremental eager hoist is not vacuous. It rules out the family of
late-only states where a visible hoist boundary is exposed and the left branch
has already taken a step under that boundary before hoisting.

Concrete witness:

- from `(((a ∧ b) ∨ d) ∧ h) σ`, late hoist may reach
  `((((a σ) × b) <-+ (d σ)) × h)`
- weak/incremental eager hoist forbids that shape, because it must hoist as
  soon as `((((a ∧ b) σ) <-+ (d σ)) × h)` becomes exposed

## Structural Summary Fold

The active `wf-*` stack now has a parallel `wf-summary-*` family.

That summary layer is the structural correctness fold for reachable
configurations. Each summary has the shape:

`(wf-summary answers bounced freshened-tree freshened-shell)`

The judgment family does two jobs at once:

- it proves the old well-formedness obligations
- it returns the structural counts that the test/support layer used to compute
  by separate host recursion

The important invariants are enforced in the judgment, not in postprocessing:

- wrapper-path scope agrees with each state's stored `c`
- lvars in goals, substitutions, disequalities, and trails stay within the
  ambient scope
- `ScopedTree` and `ScopedShell` introductions are fresh relative to the
  outer scope
- `Deferred` changes only the bounced count; it does not alter scope accounting

Operationally:

- `ScopedTree` increments the tree-freshened count
- `ScopedShell` increments the shell-freshened count
- `⊤` and answers increment the answer count
- `Deferred` increments the bounced count

This gives one reusable judgmental source of truth for exact-scope properties,
freshening accounting, and stepwise monotonicity checks.

## Picture Denotation

Visible tree construction now lives in `search-lattice/picture.rkt`.

There are two denotations over the same machine/configuration states:

- operational picture
- extensional picture

The operational picture is the current UI contract:

- it preserves `Deferred`
- it preserves the current visible branch/conjunction/delay structure
- it renders both `ScopedTree` and `ScopedShell` as the same visible
  `Freshened` wrapper, because the UI distinguishes scope extent but not the
  internal tree-vs-shell constructor name

The extensional picture erases administrative detail:

- `Deferred` is identity
- `ScopedTree` and `ScopedShell` collapse to the same visible scope
  wrapper

So the extensional picture forgets scheduler bookkeeping, while the operational
picture keeps the renderer-facing administrative structure needed for stepping
and debugging.
