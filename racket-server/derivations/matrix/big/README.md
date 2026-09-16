# Strict Big and fixed-point stages in the representation matrix

This directory supplies finite Big presentations for the twelve matrix
coordinates: Core, Delay, Disjunction, and Search, each in S, E, and N.
The full Search language additionally has three native relation-call
instances over explicit `(program Γ q)` configurations.
The [matrix contract](../README.md) owns their allocation and feature rules,
including the retained-scope invariant shared with the S reference. This
directory supplies independent inductive judgments, recursive evaluators,
and finite certificates for those same rules.

## Native equations and feature instances

`s-schema.rkt` defines the S equations over its native configuration syntax.
An explicit ordered support parameter records the enclosing active Owner
path in each premise. Answer-private Owners do not enter a residual's support.
Internal forcing retains saved Owners on the active computation before its
body premise evaluates. The independent `retain-owners` operation prepends
them to the root's Owner field, traversing only a transparent `force` wrapper.
It neither enters a delayed body nor distributes common Owners to siblings.
The E/N premises carry allocation information in their own states and failed
values, following the [source allocation rules](../README.md#allocation-representations).

`ownerless.rkt` specializes the equations separately for E and N at macro
expansion. E retains named variables and ordered `Support`; N retains
positional variables and its numeric counter. Each uses its native
kernel and allocator. Execution does not encode a row into another row.

Atomic premises use the same eager `Failure`/`Success(state)` data boundary
as the source rules.

The Search judgments describe active computation and mature residuals.
`commit-big` constructs a partial settled frontier `F` from mature Search,
stopping at `Delay` with unary `More(Delay(...))`. It neither forces that body
nor changes the strict operand schedule. `advance-big` crosses one exposed
suspension and commits its next mature chunk; `collect-big` explicitly crosses
every remaining suspension and concludes a completed observation `O`.

The comparison operation `render-big` consumes the complete Search; its
`render-*` labels distinguish constructor cases. The general `observe-big` judgment concludes partial
`F`, including complete `O`, and accepts native `commit`, `advance`, and
`collect` query contexts as well as `render`. Its value clause admits
partial frontiers without evaluating their suspended tips.

`s.rkt`, `e.rkt`, and `n.rkt` instantiate Search. `features.rkt`
instantiates the remaining nine coordinates using the corresponding
restricted source grammar. `feature-schema.rkt` removes unavailable literal
judgment rules and fixed-point constructor cases during macro expansion.
Smaller coordinates have no runtime feature switch. Public fixed-point
entries check their actual feature domain, including control constructors.

## Full programs and recursive calls

[full.rkt](full.rkt) instantiates the same S and ownerless equation schemas
with an explicit `Γ` argument on every judgment and fixed-point call. This
optional schema argument leaves the twelve call-free signatures and rules
unchanged. The full instances add one `eval-call` premise: instantiate the
named definition with its actual terms, then evaluate that body in the same
state, Owners, and relation environment. There is no automatic suspension.

`program-big/s-rel`, `/e-rel`, and `/n-rel` retain `Γ` around the complete
native result, including paused Frontiers. The callable boundaries are
`evaluate-full/s`, `/e`, `/n`, `promote-full/s`, `/e`, `/n`, and
`raw-derivations-full/s`, `/e`, `/n`; they consume `(program Γ q)`. Their
recursive operation judgments have names such as `search-big/s-rel`, with
`Γ` preceding the ordinary arguments. S still receives its inherited support
argument separately.

Each full `BigCertificate` node keeps its own `(program Γ input)` and
`(program Γ output)`. This preserves the meaning of calls inside every
premise without consulting the root or a dynamic environment. The existing
direct S→E, E→N, and S→N certificate maps preserve closed lexical definitions
while mapping that premise's native body and allocation ancestry.

These are finite inductive derivations. They cover terminating recursive and
mutually recursive programs and individual finite rounds of explicitly
suspended productive programs. Full collection of an infinite stream and
unguarded recursive proof search are not validation operations. The focused
gate checks unguarded recursion only through a bounded native reduction run;
it asserts that strict right-operand work prevents earlier commitment.

## Judgments and fixed-point evaluators

The operation judgments are `search-big/<coordinate>`,
`merge-big/<coordinate>` where disjunction exists, `bind-big/<coordinate>`,
`render-big/<coordinate>`, `commit-big/<coordinate>`,
`advance-big/<coordinate>`, `collect-big/<coordinate>`, and
`observe-big/<coordinate>`. Coordinates are
`s`, `e`, `n` for Search, or names such as `s-core`, `e-delay`, and
`n-disjunction`. S judgments additionally take the inherited support prefix.

These are mutually inductive, unbounded judgments. They give a mature Search
or exact partial/completed frontier and an ordered list of source contraction labels.
There is no transition relation, normalized machine execution, or numeric
fuel inside their premises. `promote/<coordinate>` evaluates corresponding
recursive equations directly, retaining the native Search and Frontier
constructors but omitting label traces and derivation trees. It is an
independent unbounded fixed-point evaluator, not a call to judgment search.

The strict premises evaluate the left operand, then the right operand, then
merge. Bind over `Yield` evaluates the continuation result and recursive
residual before merging. A `Delay` is a value without a premise for its body.
The [source observation and scope rules](../README.md#program-active-search-and-settled-frontier)
determine the distinct internal-force and public-resumption premises.
Conclusions preserve exact `Emit`, `Forced`, `Solo`, and `Done` structure,
including the terminal answer's Owner groups, and all native state fields.
An exposed advancement records `advance-delay`, then the resumed computation's
strict search labels, then its `commit-*` labels. It introduces no synthetic
`force-delay` event. Collection additionally
performs the recursive `collect-*` premises. Existing `Forced` prefixes are
retained through `advance-forced` or `collect-forced`; merely constructing
unary `More(Delay(...))` records no forcing event.
Delayed bind's direct body premise introduces no synthetic force either;
resuming a delayed merge still has the explicit internal-force premise.

## Finite certificates and vertical maps

`evaluate/<coordinate>` returns `(list value source-labels)`.
`raw-derivations/<coordinate>` returns Redex derivation trees without
deduplicating identical conclusions. The tests require exactly one raw
derivation for each admitted witness, including mature/pending
`Yield`, `Emit`, and `Forced` cases.

`maps.rkt` extracts `BigCertificate` from those native derivation trees. Each
node retains its operation, feature/row coordinate, inherited S prefix,
native input, native result, ordered labels, and every recursive premise.
Only presentation names of rules are omitted; the operation, input, and
premise tree determine the rule. Thus saved mature chunks and prefix-sensitive
fresh premises remain visible inside the certificate.

The exported `QBig-SE`, `QBig-EN`, and independently defined `QBig-SN` map
every node's input and result, not just the root's final observation. S maps
use that node's inherited prefix before erasing or addressing ownership.
The target E/N certificate has no implicit ownership prefix. Labels,
premise order, and the feature coordinate are preserved.

The intended vertical contract, on the source row's well-formed domain, is:

```text
QBig-SE(CertS(q ⇓ v, labels)) = CertE(Q-SE(q) ⇓ Q-SE(v), labels)
QBig-EN(CertE(q ⇓ v, labels)) = CertN(Q-EN(q) ⇓ Q-EN(v), labels)
QBig-SN(CertS) = QBig-EN(QBig-SE(CertS))
```

These equalities compare full recursive certificates. Tests obtain the
target certificate independently from its native judgment before comparing.

For each finite witness, Big's ordered labels equal the exact R trace and
the semantic labels of B's spans. Each B span is independently replayed in
M, and its final readback equals the Big conclusion. The finite-run
correspondence argument follows the Big derivation: lift operand premises
under strict contexts, perform the named local contraction, and lift the
remaining premises under the resulting constructors. There is no lift
beneath an unforced `Delay`. Conversely, a finite deterministic source run
splits at those strict context boundaries; the established B/M spans carry
the same finite run on the checked corpus. Extending that argument to the
whole admitted domain remains a proof obligation.

## Validation and scope

```sh
raco test racket-server/derivations/matrix/big/tests.rkt
raco test racket-server/derivations/matrix/big/full-tests.rkt
```

The gate checks mature Search and complete observations across the native
feature rows, exact source/B labels, M span replay, fixed-point agreement,
raw proof uniqueness, and recursive QBig squares/composition. Targeted cases
cover intermediate nested delayed-merge states, inherited and sparse support, shared
versus answer-local ownership, constructor overlaps, and absent-feature
rejection.

The [named validation witnesses](../../test-support/witnesses.rkt) exercise
partial value proofs, commit, advancement, and explicit collection at every
exposed boundary. These are compared with exact R/B results and label traces,
fixed-point equations, unique raw proofs, and full recursive S/E/N certificate
maps. Further cases assert exact forcing labels and exercise public operations
throughout the feature/row instances. Complete results are also compared with
the full-consumption `render` operation.

Retained-scope regressions check allocation and attachment order and inspect
the internal-force certificate's exact active body premise. Bind, choice,
and nested-force roots retain the saved common Owner group without placing
it on siblings. Other cases check direct resumption in delayed bind and
public render, and reject obsolete `prefix` computations in every feature
and row. Delay and Search exercise internal forcing in all three native rows.

The full gate additionally compares native recursive Big proofs, fixed-point
results, source labels, and R/D/Z/M/B stages at public boundaries. Its cases
cover direct and mutual recursion, lexical shadowing, relation bodies that
allocate before and after Delay, nested delayed merges with pending bind, sparse
ancestry, unused introductions, and finite productive rounds. Every recursive
certificate retains and maps the explicit relation environment.

The unbounded inductive presentations are not bounded interpreters. Their
finite witness gate is evidence for the stated correspondence contracts, not
a general mechanized preservation, adequacy, productivity, or coinductive
stream proof. A fusion with the earlier dormant-branch semantics remains
outside these coordinates, as do No Interleave/Railroad Big instances.
These finite Big certificates do not derive new E/N functional
or register machines. The retired numeric Big/proof-search results are
recorded separately in the [correction log](../../CORRECTIONS.md#retired-and-deferred-results).
