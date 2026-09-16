# S reference: interpreter and corresponding machines

This directory contains the S reference: an independently stated strict
interpreter and reduction semantics, their two derivations, and the machine
checkpoint used to check the S/E/N matrix. It uses named logic variables with
introductions stored on the nodes that own their scope.

Retained scope names an invariant of this account: when internal force removes
a Delay, its introductions remain on the running computation before the body
evaluates. The parent [research guide](../README.md) owns the strict Search
coordinate inventory, generation commands, and reading order.

Both derivations use `Yield(O,A,S)` for the eager active Search cell and
reserve `More(Delay(O,R))` for unfinished Frontier work. The eager-tail frame
is `KMergeYield`/`yield`; the corresponding source cases are `mplus-yield`,
`bind-yield`, `render-yield`, and `commit-yield`. These names identify the same
operations and stopping boundaries in both presentations.

`One(O,σ)` is an uncommitted Search candidate. Commitment produces
`Solo(O,σ)`, whose terminal answer and introductions are carried directly on
the Frontier node. `Answer(O,σ)` remains the head payload of `Yield` and
`Emit`, where its private introductions must be distinguished from the
common introductions on the enclosing cell.

Common grammars, kernels, stage construction and control transformation live in
[shared/](../shared/README.md); fixtures and reusable checks live in
[test-support/](../test-support/README.md). Runtime and derivation modules
depend on those shared implementations directly. The native S/E/N matrix
uses the same source factoring. Its independently stated S equations and
stages are checked against this checkpoint; the functional interpreter does
not execute any matrix source or machine.

## Inspectable path from syntax to interpreter

The source-guided reconstruction can be inspected in this order:

1. [source.rkt](source.rkt): the strict source equations and owner lifting.
2. [stages.rkt](stages.rkt): the source instantiated in the existing
   decomposition/refocusing construction, giving D, Z, M, and B.
3. [cps.rkt](cps.rkt): the specialized functional continuations corresponding
   to those strict operations and contexts.
4. [interpreter.rkt](interpreter.rkt): direct-style equations with those
   continuations discharged through ordinary calls and returns.

The functional pipeline is:

```text
interpreter.rkt → cps.rkt → data.rkt + defunc.rkt → machine.rkt
                                                    │
                                             functional->M
                                                    │
source.rkt → decomposition → refocused M ─────────────┘
```

The direct/CPS reconstruction and defunctionalization are manual passes.
[derive.rkt](derive.rkt) mechanically generates the first-order machine from
the actual defunctionalized control bodies. Neither functional evaluator calls
a reduction relation or another machine driver.

[machine-correspondence.rkt](machine-correspondence.rkt) maps functional
configurations directly into native refocused controls and continuations.
[readback.rkt](readback.rkt) independently reconstructs whole source terms.
[CORRESPONDENCE.md](CORRESPONDENCE.md) owns the configuration relation, frame
mapping, prescribed source-operation spans, and administrative progress measure.
The checks compare complete intermediate configurations; universal domain
preservation and correspondence remain obligations.

## Relation definitions and calls

The maintained extension accepts relation environments
`Γ = ((r:name (x:parameter ...) goal) ...)` and calls
`(r:name argument ... tag)`. Calling `run` with `#:relations Γ` selects this
full language and returns `(program Γ Frontier)`, including when Γ is empty.
Omitting that keyword retains the existing call-free interface. Public
`resume-once` and `collect-all` recognize and preserve the program boundary.

Calls substitute the actual arguments into the named body and immediately
evaluate that body under the same Owners and state. Their source label is
`eval-call`. They do not create a Delay: recursive productivity depends on
explicit suspension in the program produced by the compiler.

[relations.rkt](relations.rkt) supplies the explicit `ProgramGoal(Γ,goal)`
capture used by pending evaluations, right operands, `GRight`, and `REval`.
The direct and CPS interpreters capture that data lexically. At the data
stages `KProgram(Γ,k)` keeps the environment present while eager merge/bind
or commitment runs and reconstructs `(program Γ Frontier)` on return.
It maps directly to the syntactic machine's program frame. Environment data
does not use dynamic parameters or an evaluator hidden behind readback.
Structural configuration validation checks every pending capture against
its program boundary and rejects missing or changed environments.

[relation-tests.rkt](relation-tests.rkt) checks exact source operations,
native S/E/N configuration maps, direct/CPS atomic work and paused Frontiers,
generated-machine/register transitions, and prescribed compressed spans.
Its witnesses include named calls, parameter shadowing, mutual recursion,
bounded explicit-delay recursion, deliberately unguarded right recursion,
nested delayed merges, retained unused allocations, sparse ancestry, and eager bind.

## Registerization and first compression

The corresponding functional machine is the reference for two further
executable stages:

```text
machine.rkt ← decode ← registers.rkt ← structural embedding ← compressed.rkt
```

[register-derive.rkt](register-derive.rkt) produces [registers.rkt](registers.rkt);
each register step decodes to one functional-machine step. The host dispatch
loop remains separate from object-language Delay.
[compression-derive.rkt](compression-derive.rkt) produces
[compressed.rkt](compressed.rkt), replacing three atomic dispatches with one.
The resulting `return/d` still has its pending continuation. Calls and
program-boundary returns retain their original single-step spans.

[REGISTERIZATION.md](REGISTERIZATION.md) owns the register layout, structural
decoders, prescribed one- or three-step spans, termination argument, and
concrete before/after example. All continuation and resumption families remain;
this compression does not derive a compact κ/Q/π machine or separate E/N
register programs.

## Retaining introductions on active computation

The [matrix source contract](../matrix/README.md#program-active-search-and-settled-frontier)
records the shared lifting equations and the distinction between internal
force and public advancement. Internal force places Owners on the running
root; public advancement retains them on Forced. The independently stated
[S reference source](source.rkt) follows those same rules. Its `eval`, `mplus`,
and `bind` controls retain ownership while operands run, so neither derivation
needs a pending `prefix` operation or continuation.

## What interpreter the source suggests

The three possible delayed bodies created from goals are initially rooted at
`eval`, `mplus`, or `bind`, with empty local Owners. Each body therefore becomes
a function accepting the root Owners O and inherited support P at entry:

```text
REval(g,σ)(O,P)
  = eval(g,σ,O,P)

RMerge(right,left)(O,P)
  = mplus(right, force(left,P ++ names(O)), O,P)

RBind(r,f)(O,P)
  = bind(r(empty,P ++ names(O)), f,O,P)
```

These equations follow the three source roots. RMerge evaluates the required
force before applying mplus; RBind evaluates its operand before applying bind.
The saved right Search in RMerge has already matured, as required by strict
disjunction.

Internal force and public advancement use different placements of the same
scope:

```text
force(Delay(O,r),P)
  = r(O,P)

advance(More(Delay(O,r)),P)
  = Forced(O,commit(r(empty,P ++ names(O))))
```

The corresponding CPS force tail-calls `r(O,P,k)`. It has no continuation
waiting to prefix the returned value. The root ownership is already present
in the computation, and an ordinary merge or bind continuation retains it
when an operand is being evaluated. Defunctionalization produces the ordinary
KMergeForced/KBindForced frames carrying that root ownership, and preserves
KCommit and strict operand continuations. There is no KPrefix data constructor.

Direct resumptions accept `(O,P)`;
CPS resumptions accept `(O,P,k)`. They are context-parameterized suspended
computations. Only Delay suspends search work. They do not cache an inherited
`here` value. Public consumers instead reconstruct inherited support by
following common Owners through Emit and Forced, excluding answer-private
Owners. Ordinary evaluator calls still use a derived support list and the
existing fresh-name kernel.

## Representation maps and remaining domain argument

The active-computation obligation is ownership equivariance. If c steps at
inherited support `P ++ names(O)`, `lift_O(c)` should step with the same label
at P to the lifted successor. Fresh sees the same ordered support; the other
active cases use associativity of owner concatenation. Internal force uses
`lift_O(lift_L(c)) = lift_(O ++ L)(c)`. The rule argument and finite allocation
checks support this lemma; they are not a mechanized universal proof.

This argument concerns active computations. Commitment remains an explicit
phase even where it preserves the ownership field: `commit(One(O,σ))`
produces `Solo(O,σ)`, while committing `Yield` retains its separate common and
answer-private introductions on `Emit` and its `Answer` payload. A candidate
with pending bind obligations is not yet a committed answer.

The existing S→E/N maps account for allocation along owner paths. At the same
caller support the basic identity is
`Q(lift_O(V),P) = Q(V,P ++ names(O))` for mature Search.
[machine-correspondence-tests.rkt](machine-correspondence-tests.rkt) checks
structural squares for complete configurations. The matrix's
[representation-map contract](../matrix/README.md#direct-representation-maps-and-domain)
owns the native S/E/N domain and checkpoint checks. Those checks connect this
functional machine to actual native S/E/N transitions, including direct S→N;
they do not derive separate E/N functional interpreters or register programs.

## Examples and validation

[show.rkt](show.rkt) displays internal force and the native bind frame that
retains an introduction while its operand runs; the later fresh variable
must account for that allocation. [show-machines.rkt](show-machines.rkt)
displays corresponding functional and refocused configurations and checks
their prescribed transition diagram.
[show-register-compression.rkt](show-register-compression.rkt) prints paired
atomic success/failure traces with commitment still pending.

[source-tests.rkt](source-tests.rkt) checks native R/D/Z/M/B transitions,
allocation support, owner lifting, and exact incremental Frontiers.
[interpreter-tests.rkt](interpreter-tests.rkt) compares direct/CPS boundaries,
suspended bodies, and actual atomic work. Its test observer records real
closure captures without executing a Delay.
[defunc-tests.rkt](defunc-tests.rkt) extends those checks to first-order data
and generated machine execution.
[machine-correspondence-tests.rkt](machine-correspondence-tests.rkt) checks
every reached configuration, prescribed source labels, administrative rank,
constructor coverage, and invalid ancestry/phase rejection.
The [register tests](register-tests.rkt) and
[compression tests](register-compression-tests.rkt) check decoding, update
order, exact one/three-step spans, and work preservation.
[relation-tests.rkt](relation-tests.rkt) extends those checks to explicit
program environments and recursive calls without weakening observations.

The [shared witnesses](../test-support/witnesses.rkt) cover empty and unused
introductions, sparse ancestry, fresh across Delay, existing variables,
saved-right reuse, eager bind, terminal structure, and nested delayed merges.
[all.rkt](all.rkt) aggregates this account's checks, including
freshness of its three generated programs; commands are in the
[parent guide](../README.md#generated-artifacts-and-reproduction).

These are finite checks of the S reference and its downstream
transformations. General domain preservation, administrative progress on the
native side, universal S/E/N correspondence, productive streams, and compact
κ/Q/π rail compression remain obligations. The aligned
[matrix Big](../matrix/big/README.md) supplies finite judgments and certificate
checks for these source rows; it does not add a separate register-to-Big map
or a productive-stream theorem.
