# Strict downstream matrix stages

These stages derive D/Z/M/B from the matrix's strict source equations in all
twelve native S/E/N feature cells. [The matrix contract](../README.md) owns
their allocation and feature rules; this document describes the stage
configurations, transitions, maps, and validation.

`instances.rkt` instantiates the common construction in
[`shared/stages/schema.rkt`](../../shared/stages/schema.rkt) directly
over the native strict S, E, and N sources, preserving their
[allocation representations](../README.md#allocation-representations).
No stage executes by converting its configuration to another representation.

Each feature uses its own literal source rule set, recursive goal/computation
grammar, and value/observation predicates. The exported native instances are:

| Feature     | S              | E              | N              |
|-------------|----------------|----------------|----------------|
| Core        | `SCore`        | `ECore`        | `NCore`        |
| Delay       | `SDelay`       | `EDelay`       | `NDelay`       |
| Disjunction | `SDisjunction` | `EDisjunction` | `NDisjunction` |
| Search      | `S`            | `E`            | `N`            |

Relation calls are outside these twelve call-free instances; `full.rkt` adds
them separately, as described below. `tests.rkt` exercises all twelve
coordinates at every downstream stage, including exact feature-inclusion
edges and rejection of absent constructors. Lower coordinates do not merely
run a restricted goal corpus using the full Search machine. Even explicitly
constructed D/B terminal states are checked against their feature's value
grammar before being accepted as complete.

| Stage | State and transition |
| --- | --- |
| D | Canonical redex and frame list; contract, reconstruct whole source, and decompose again. |
| Z | Control and retained frame list; one source contraction or one administrative descent/ascent. |
| M | Control and nested first-order continuation; independent direct transition equations, with structural Z/M inverses. |
| B | Canonical residual dispatcher; one source contraction followed by structural administrative normalization, with the complete exact M label span. |

Each row supplies its local source contraction, a pure constructor view, and
its support-prefix operation. These are shared language/kernel boundaries,
not calls to another stage's transition. Source context closure remains an
independent reference for exact labeled successor comparisons.

Those host operations form a fixed implementation descriptor; they are not
stored in D/Z/M/B semantic configurations. Runtime controls and continuations
are first-order data. Atomic contraction uses the native
`Failure`/`Success(state)` boundary described with goals, program environments,
active Search, and settled Frontiers in the
[matrix contract](../README.md#program-active-search-and-settled-frontier).

The observation boundary is derived from the source context `commit E`.
The native view descends through it with a `commit` Frame, which D retains
in its frame list and Z/M retain while refocusing. Eager output construction
similarly produces `emit` frames. Public `advance` and `collect` have their
own source contexts and frame kinds. These views operate on native source
syntax and do not import the functional machine.

The [internal-force and public-advance rules](../README.md#program-active-search-and-settled-frontier)
produce ordinary eval, mplus, bind, and Forced controls and frames. These
retain the required ancestry; there is no separate prefix Frame or K
constructor. The E/N instances preserve the same named operations while
carrying allocation scope in their native states and failed values.

`More(Delay(...))` is a native Frontier normal form in every feature that admits
Delay. D returns DFinal, Z/M finish with empty continuations, and B returns
BFinal at that value without forcing it. Public resumption is a new source
computation, `advance F`; it is not an external cut in a collect-all run.

S allocation reads the ordered owner prefix on the active ancestor path.
Frame metadata records only each ancestor constructor's Owners. Answer-only
Owners and sibling Owners never enter that prefix. Tests compare the retained
frame calculation with the source's separate context calculation at every
reachable zipper state, including sparse initial support and an answer-local
allocation that must not affect the residual.

[Stage maps](../../shared/stages/maps.rkt) define direct S-to-E, E-to-N, and
S-to-N maps at D, Z, M, and B.
They map controls and retained frame payloads without reconstructing or
re-decomposing a source term. The E-to-N map addresses a retained bind goal
using the support prefix common to its possible inputs. Yield heads and choice
siblings keep their individual supports. Their union is never treated as a
shared allocation scope.

Compression removes administration only. Each nonempty span contains one
source label followed by the exact administrative labels needed to reach the
next residual. A B step cannot hide unbounded eager search work. Delay
production, forcing, and observer events remain separate source-labeled
edges. The independent `b-step/spec` composes exact M edges, while
`replay-span` checks their order and endpoint. The executable B dispatcher
calls neither that specification nor M/Z transitions.

## Full relation programs

[full.rkt](full.rkt) adds `SRel`, `ERel`, and `NRel` by instantiating the same
stage construction with the full native sources. A `program` frame keeps Γ
in the configuration while eval, merge, bind, or observation work runs.
Named and recursive calls use the source's `eval-call` contraction; the
stage extension introduces no automatic Delay.

The exported `s-rel-status`, `e-rel-status`, and `n-rel-status` inspect the
next phase without running a kernel or expanding a call. They distinguish
`running`, `paused` at an unfinished Frontier, `complete`, and `stuck`.
[Full-program checks](../full-tests.rkt) exercise D/Z/M/B, exact public
boundaries, recursive calls, and S/E/N maps. These are the reference Search
stages; No Interleave and Railroad have no separately derived downstream
instances here.

## Validation and domain

Run the downstream matrix checks with:

```sh
raco test racket-server/derivations/matrix/stages/tests.rkt
raco test racket-server/derivations/matrix/stages/domain-tests.rkt
raco test racket-server/derivations/matrix/stages/commit-tests.rkt
raco test racket-server/derivations/matrix/full-tests.rkt
```

The checks cover every intermediate labeled successor, source readback,
Z/M inverse and transition equation, B direct/specification/exact replay
square, direct vertical map composition, and tampered compression
certificates. The gate also uses independently generated,
scope-aware mixed goals at depth four/five, each from both empty and sparse
ordered initial support with aliasing and disequalities. Every generated
path checks native intermediate states and all three direct vertical maps.
The domain gate additionally checks absent values in manually constructed
terminal states and the retained mature left chunk while the right operand
is still evaluating, in both M and B.

The commitment gate runs commit/advance/collect through R/D/Z/M/B and all
three direct representation maps, including every boundary of the named
validation witnesses. Partial results must terminate by each stage's own
rules. The gate also checks the absence of prefix frames, sparse allocation
beneath retained active Owners, and the separation of force,
resumed evaluation, and commitment at every stage and in all three
configuration representations.

[The checkpoint gate](../s-reference-tests.rkt) additionally compares the
matrix S stages to the S reference's independently instantiated stages,
then checks the S reference functional machine against actual native S/E/N M
steps through its structural configuration maps. Its administrative spans
cannot skip source work, and its native B steps must report and replay the
same exact M spans. No separate E/N functional or register derivation
is implied by this native machine connection.

These check executable correspondence over the supplied corpora. They are not
a general adequacy, guardedness, or coinductive productivity proof.
Administrative normalization is defined over
finite, well-formed reachable controls and contexts; forged frame metadata is
outside that domain.

Construction provenance: the original decomposition, retained-refocusing,
machine reification, and direct/specification compression organization was
inspected at repository commit `229bb0cd277d53533f76a5872cd35e88938fa932`
(`codex/whole-tree-redex-column`), principally
`racket-server/derivations/refocusing/whole-tree-redex-column/{decomposition,machine,compressed,compression-spec}.rkt`
and
`racket-server/derivations/refocusing/whole-tree/reference/marked/machine-schema.rkt`.
Those earlier dormant-branch transition rules are not copied or imported here. The
strict constructor views, native stage implementation, and stage maps are
strict instances of the reusable construction ideas.
