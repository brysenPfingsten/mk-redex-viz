# Strict representation and feature matrix

These native S/E/N feature instances use the same strict operations and
allocation-scope invariant as the S reference source and interpreter in
[s-reference/](../s-reference/README.md). Common grammars, kernels,
representation maps, and stage construction live in [shared/](../shared/README.md);
this directory owns the matrix's source and Big rules and their instances.
The [research guide](../README.md#sen-coordinate-inventory) owns the complete
coordinate inventory and remaining obligations. Source and R/D/Z/M/B/Big
equations use this factoring in all twelve feature cells. The
[checkpoint gate](s-reference-tests.rkt) connects the independently stated
Search S source and stages to the S reference functional machine and the
native E/N machine transitions.

The matrix uses `strict-round` branch evaluation in each S/E/N representation.
Each row evaluates both operands of `mplus` before merging, constructs eager
`Yield` tails, and evaluates both continuation and residual work in `bind`.
Only `Delay` retains a suspended computation. The source equations live here;
the variable, state, allocation,
unification, and disequality machinery uses the live shared providers in
[shared/core/](../shared/core/PROVENANCE.md), whose historical origin is
recorded there.

Each row's atomic kernel directly returns first-order `Failure()` or
`Success(state)` data. Source and Big-step equations match that result to
construct their native Search values. Kernel work is eager; no callback is
stored in an outcome and no result adapter is involved. Primitive unifiers
retain their existing internal results. Native source, stage, and Big checks
obtain results independently within these matrix presentations.

## Program, active Search, and settled frontier

These are different semantic roles, independent of the S/E/N representation
coordinate:

| Role | Matrix syntax | Operational responsibility |
| --- | --- | --- |
| Goal | `g`: atomic goals, fresh, conjunction, disjunction, suspend; calls in the relation extension | Describes work to evaluate against a state. A disjunction in `g` is not a computed answer or a scheduler residual. |
| Program | `(program Γ q)` in the relation extension | Keeps relation definitions `Γ` around the running computation or its Frontier. |
| Active Search | `c`: eval/mplus/bind/force computations; mature `SV`: Empty/One/Yield/Delay | Retains strict work and mature eager chunks that merge/bind may still process. A mature candidate is not automatically committed output. |
| Settled frontier | `F`: Done/Solo/Emit/Forced and unary More(Delay); `O` is the completed subset | Contains committed answers and at most one explicit unfinished tip. It is a normal form even when that tip is pending. |

Observation computations `o` include `commit c`, `advance o`, and `collect o`.
The public query starts at `commit(eval(goal,state))`. The context `commit E`
matures its Search operand strictly before any commitment rule applies.
Commit converts Empty/One/active Yield to Done/Solo/Emit, but converts Delay
to unary `More(Delay)` without forcing. There is no evaluation context under
that unfinished-work wrapper or under Delay.

`advance F` preserves the settled prefix and crosses one exposed Delay;
`collect F` explicitly consumes all remaining exposed Delays. Commitment
performs no forcing; the consumers have explicit crossing rules. Their
contexts and rules are part of each native source, not test-runner stopping
conditions.

The functional derivations and this matrix use `Yield` for active Search and
`More(Delay(...))` for unfinished Frontier work. Their structural maps preserve
these constructor names while mapping allocation information and resumption
representations. An active Yield is neither program disjunction nor settled
output. The matrix retains strict operand evaluation and eager Search tails.

The S `commit-one` rule marks the commitment boundary:
`One(Owners,state)` becomes `Solo(Owners,state)`. The terminal answer owns its
introductions directly; no separate empty terminal shell is admitted by the
current grammar. E/N use `Solo(state)`. The full-consumption `render-one` rule
uses the same placement. Commit of
an eager tail preserves common and answer-private ownership; public advance
retains a crossed Delay's Owners on Forced and commits the unprefixed body.
Active merge/bind and internal force retain their own role and strict order.

Public advance/collect/render expose the stored body directly, as does the
resumption stored by delayed bind. Internal force instead retains the removed
Delay's introductions on the active body:

```text
S:   force(Delay(O,c)) → lift_O(c)
E/N: force(Delay(c))   → c
```

Let `O ++ L` concatenate introduction groups, preserving names, order,
group boundaries, and tags. The complete S root-lifting operation is:

```text
lift_O(eval(L,g,σ))    = eval(O ++ L,g,σ)
lift_O(mplus(L,c₁,c₂)) = mplus(O ++ L,c₁,c₂)
lift_O(bind(L,c,g))    = bind(O ++ L,c,g)
lift_O(Empty(L))       = Empty(O ++ L)
lift_O(One(L,σ))       = One(O ++ L,σ)
lift_O(Yield(L,A,c))   = Yield(O ++ L,A,c)
lift_O(Delay(L,c))     = Delay(O ++ L,c)
lift_O(force(c))      = force(lift_O(c))
```

It prepends O to the active body's root Owners and passes through a `force`
wrapper. S allocation therefore sees those introductions while the body runs.
Common introductions are never distributed individually onto the operands
or mixed with answer-private introductions. E/N already carry the corresponding
allocation scope in states and failed values.

Public advancement crosses only the exposed Frontier tip:

```text
advance(More(Delay(O,c))) → Forced(O,commit(c))
```

The retained Forced already carries O; lifting its body as well would duplicate
the introduction. Collection and delayed bind also preserve their common
Owners around the body. These rules are independently stated in the
[S reference source](../s-reference/source.rkt); its
[resumption equations](../s-reference/README.md#what-interpreter-the-source-suggests)
show how the same scope discipline appears in the interpreter.

There is no syntactic `prefix` computation, `prefix-value`
contraction, or prefix frame in any row. The functional interpreter still
uses a `prefix` helper to attach Owners to an already mature Search; that
helper does not resume work or derive a pending continuation.

The `render c` operation is an explicit full-consumption observer
for comparison. It can resume repeatedly and is not the partial public query.
Finite equality with `collect(commit c)` is checked independently. This
extension introduces no fusion, scheduler, or branch-evaluation policy.

## Allocation representations

Lexical-variable syntax is shared: a fresh binder allocates runtime logic
variables and substitutes them for its own bound variables. S/E/N differ in
their runtime logic-variable domains and in where configurations retain
allocation information. In the named representations, allocated-name support
is the complete ordered collection of names already allocated at the current
computation point, including unused names. An `intro` group records only the
names introduced by one fresh binder.

| Property | S | E | N |
| --- | --- | --- | --- |
| Runtime logic-variable domain | Named `u:*` atoms | Named `u:*` atoms | Bare natural levels; numeric data is `(nat n)` |
| Logical state | `(state sub dis trail tag)` | `(state (Support u ...) sub dis trail tag)` | `(state next sub dis trail tag)` |
| Allocation information | Ordered tagged `(Owner intro tag)` groups at computation/value/observer positions | Ordered state-local allocated-name `Support` | State-local next level |
| Allocation information after failure | `(Empty Owners)` / `(Done Owners)` | `(Empty Support)` / `(Done Support)` | `(Empty next)` / `(Done next)` |
| Fresh | Least unused canonical names from the active scope | Least unused canonical names from state `Support` | Consecutive interval starting at next |

S never stores cumulative support in its logical state. Its strict constructors
are:

```text
eval   ::= (eval Owners g state)
merge  ::= (mplus Owners c c)
bind   ::= (bind Owners c g)
Search ::= (Empty Owners)
         | (One Owners state)
         | (Yield Owners (Answer Owners state) Search)
         | (Delay Owners c)
```

Owners on `mplus`, `bind`, `Yield`, and `Emit` are ordered common introduction
groups. An `Answer`'s Owner groups belong only to that answer; they never reserve names in
its residual or sibling. Source allocation reads exactly the Owner groups
along the active strict evaluation context. Empty binders retain an empty,
tagged Owner group and an `allocate-fresh` transition.

E and N erase those Owner positions and have ordinary ownerless strict
`eval`/`mplus`/`bind`/`Yield`/`Delay` constructors. The successful state carries
the cumulative supply. Failure retains only the supply summary, never the
failed substitution, constraints, trail, or state tag. Siblings inherit the
same allocation scope independently and may allocate the same name/level after
splitting.

## Feature ownership and scheduler policy

Every feature has a separate recursive goal grammar, value/observation grammar,
and statically generated Redex rule set in each representation:

| Feature coordinate | Goal and configuration syntax added | Strict rule count |
| --- | --- | --- |
| Core | Atomic goals, lexical fresh, conjunction; Empty/One, Done/Solo, eval/bind and observation operations | 13 |
| Delay | suspend; Delay/force/Forced, unfinished More(Delay), explicit crossing | 22 |
| Disjunction | disjunction; active Yield/mplus/Emit | 22 |
| Search | Both features plus delayed merge | 32 |

These counts describe unique rule labels, not test passes. Lower rows reject
absent constructors even inside a mature value or observer; they do not merely
disable source-goal parsing. `../shared/feature-schema.rkt` selects literal clauses and
grammar productions at expansion time. It does not gate a full relation with
a runtime feature predicate.

The combined call-free feature is **Search** (`search` in the feature schema).
Other project documents also call it **Search/rail**. That historical name
refers to the reference delayed-merge behavior, not the GUI's **Railroad**
extension with explicit right-oriented syntax. Search's `mplus-delay` rule
is an explicit interaction beyond the union of the two child rule sets. This is
where the direct interpreter's delayed-merge policy acts. The scheduler-neutral
literal Search union belongs to the earlier dormant-branch semantics;
it is not asserted as a law of this strict interpreter coordinate.

## Direct representation maps and domain

`../shared/maps.rkt` independently defines `Q-SE`, `Q-EN`, and direct `Q-SN`. S maps
accumulate Owner introductions along each allocation-scope path; E maps address
names by their positions in the ordered Support, including sparse and unused entries.
`Q-SN` never calls the other two maps. No source coordinate executes by
converting itself to N and invoking another evaluator.

`../shared/wf.rkt` checks grammatical membership, duplicate-free ordered
allocated-name support, runtime-variable coverage, acyclic substitution,
lexical scope, numeric bounds,
exact unification-trail replay from the empty substitution, and the support
available to future bind goals. Trail replay uses each preserved row kernel
and must reproduce the stored substitution including its order and alias
structure. Sparse initial stores therefore include their complete alias trail.
S future goals may use only their enclosing allocation scope. E future goals
are checked against the common ordered support prefix of the states and failed
values reaching them. Named support alone does not recover erased allocation
ancestry: the intended prefix-coherent domain
is reachable configurations from well-formed roots and their explicit S/E/N
translations, with future goals referring only to inherited variables. A raw
grammar match is not a proof of that domain.

### How strong should WF be?

The early refactoring notes asked:

> Do I want those triangle properties enforced in the wf- check in the model?
> It may be overkill for the properties we want to demonstrate and make testing more difficult.

The note leaves "triangle properties" unspecified. The current matrix requires
acyclicity and exact trail replay. The question to retain is
which WF conditions each intended result actually needs, and whether their
strength is justified. The implementation and
[replay checks](tests.rkt) continue to enforce the full contract described above.

### Executable domain and correspondence checks

The core corpus includes empty, unused, multiple and shadowing fresh binders;
aliases, occurs checks and disequality failure; and allocation followed by
failure. Feature corpora add independent sibling allocation, shared outer
variables, eager bind tails, Delay-only behavior, and nested delayed merges.
The source suite checks exact named successor multiplicities, complete label
traces, all three vertical maps and direct composition at every reached state.
[property-tests.rkt](property-tests.rkt) extends those checks over generated
lexical goals using the native S/E/N rows.

[s-reference-tests.rkt](s-reference-tests.rkt) checks the independently
stated S reference and matrix S sources and D/Z/M/B configurations at each edge.
It then maps the S reference functional configuration into native S/E/N M
configurations and checks actual native steps: one preclassified source
operation with only administrative normalization around it. Native B steps
must report and replay that exact M span. Direct S→N and
S→E→N remain separate checks. These checks include commit, advance, collect,
exact intermediate Frontiers and suspended bodies. They derive no separate
E/N functional interpreter or register program.

These are bounded executable correspondence checks. They do not constitute
universal adequacy, naturality, guarded fusion, or productive-stream proofs.
The source's `eval-atom` label is its strict atomic contraction; preserved
kernel operations do not imply identity with the work trace of the earlier
dormant-branch semantics.

## Full relation programs

[full-source.rkt](full-source.rkt) extends Search in all three allocation
representations with named relations, recursive and mutually recursive calls,
and an explicit `(program Γ q)` environment. Call expansion is a named
`eval-call` step. It substitutes actual arguments without adding a Delay;
suspension comes from the program or the selected compilation profile.

[stages/full.rkt](stages/full.rkt) supplies SRel/ERel/NRel through D/Z/M/B.
Their program frames retain Γ as data. Its status predicates inspect the next
syntactic phase without performing kernel work: `running`, `paused` at a
Frontier ending in `More(Delay(...))`, `complete`, or `stuck`.
[big/full.rkt](big/full.rkt) gives independent finite equations and certificates
whose recursive premises retain the same environment.

[full-tests.rkt](full-tests.rkt) checks exact source and stage edges, S/E/N
maps, well-formedness, finite and mutual recursion, bounded productive rounds,
unguarded calls, lexical shadowing, fresh across Delay, sparse ancestry, nested
delayed merges, and pending bind. [big/full-tests.rkt](big/full-tests.rkt) checks the
finite judgments and direct certificate maps. The S reference functional route's
[relation checks](../s-reference/relation-tests.rkt) connect its independently
derived machine to these actual native configurations. This adds three full
language instances above the twelve call-free representation/feature cells;
it does not introduce separate E/N functional pipelines.

The Strict reference (Flip) and Flip-Flop GUI selections execute `strict-s-rel-red` from
this module directly; the compiler supplies their common initialization.
The twelve compiler profiles (associativity and delay placement) are distinct
from the matrix's twelve call-free representation/feature cells.

## Files and interfaces

The GUI's three schedulers now use [scheduler-source.rkt](scheduler-source.rkt),
an extension of this strict S source. **Flip-Flop** (`flip`) is the existing
`strict-s-rel-red` itself. **No Interleave** (`dfs`) changes its delayed-merge
equation to retain left priority. **Railroad** (`rail`) adds `mplusR` and eager
`YieldR` syntax to retain orientation when scheduling switches sides. This is
an S source extension; separate No Interleave/Railroad E/N rows and downstream
derivations have not been generated.

All policies mature both merge operands and eager bind residuals before
commitment. For right-oriented merge, the right operand is the first logical
argument: evaluate right, then left, then merge. `YieldR` matures its tail
before becoming a Search value. There is no evaluation beneath Delay or
Frontier More. The equations abbreviate No Interleave as DFS and Flip-Flop
as Flip, and omit Owners:

```text
DFS:   mplus(Delay c, S) → Delay(mplus(force(Delay c), S))
Flip:  mplus(Delay c, S) → Delay(mplus(S, force(Delay c)))
Rail:  mplus(Delay c, S) → Delay(mplusR(force(Delay c), S))
       mplusR(S, Delay c) → Delay(mplus(S, force(Delay c)))
```

`erase-orientation` maps `mplusR(L,R)` to `mplus(R,L)` and `YieldR(T,A)`
to `Yield(A,T)`, retaining Owners, definitions, goals and states literally.
[scheduler-tests.rkt](scheduler-tests.rkt) compares each Railroad successor
list against exactly one reference Flip-Flop step after the explicit rule-name
map, including retained-scope witnesses, eager oriented bind and relation
calls. This map is used for inspection/WF/status and to read the active Owner
path from an allocation context. Execution uses native Railroad rules, never
a Flip-Flop reduction of a converted configuration. Universal correspondence
is still a proof obligation.

The renderer's arrows show merge orientation; its highlighted edge shows
the operand currently being matured. These need not select the same child.
Every scheduler uses explicit public `advance`, while `force-delay` remains
an internal reduction. Scope allocation follows this matrix's active Owner
path. The [earlier dormant-branch account](../experiments/dormant-branch-semantics/README.md)
keeps its comparison sources, derivation and tests under `experiments/`;
it is not the GUI's runtime provider.

- `source-s.rkt`, `source-e.rkt`, `source-n.rkt`: call-free Search source rows.
- `full-source.rkt`, `full-tests.rkt`: full relation-program extension and checks.
- `features.rkt`: separately generated Core, Delay and Disjunction rows.
- `../shared/maps.rkt` and `../shared/wf.rkt`: direct source maps and executable domain checks.
- `kernel-tests.rkt`: native data outcomes, complete State preservation,
  unification/disequality results and failures; imported by the source gate.
- `commit-source-tests.rkt`: native commitment and public resumption rules,
  partial normal forms, ownership, and direct S/E/N squares.
- `stages/`: decomposition, refocusing, machine, compression, and stage maps;
  its README records the stage-specific construction and evidence.
- `big/`: direct finite Big equations over the native configuration representations.
- `property-tests.rkt`: generated-goal representation and source checks.
- `s-reference-tests.rkt`: S reference checkpoint and actual S/E/N source
  and machine transition checks.
- `all.rkt`: aggregate source, feature, property, stage, and Big checks.

Each source exposes its own named relation, `contract`, Search-value,
complete-observation, and partial-frontier predicates. `initial` constructs
an eval computation; `query-initial` constructs its public commit computation.
`s-contract(redex, prefix)` accepts the active scope's ordered allocated-name
support; S stages derive that prefix from their actual Owner-bearing frames.
E/N contracts accept the same optional
argument and read supply from their own focused state. Public source run/trace
budgets reject negative values; exhaustion is an error, not a terminal value.

Run the full matrix aggregate from the repository root:

```sh
raco test racket-server/derivations/matrix/all.rkt
```

The focused source/feature, [stage](stages/README.md), and [Big](big/README.md)
suites remain independently runnable. A source-only run does not establish
the complete horizontal pipeline. The gate checks the S reference's connection
to native S/E/N sources and machines over finite witnesses; it does not establish
a universal machine correspondence or productive streams. Relation programs have the separate
finite and bounded checks described above; these are not universal proofs.
