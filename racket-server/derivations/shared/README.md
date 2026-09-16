# Shared semantic and derivation machinery

This directory contains implementations used by the S reference
account and the native S/E/N matrix. It contains no account's evaluator,
contraction relation, or generated program. Consumers import these modules
directly; the old utility locations have no forwarding stubs.
Some experiments and application modules also consume these primitives.
That reuse does not place their callers' source semantics in this directory.

Active Search uses `Yield`; unary Frontier `More` holds an unfinished Delay.
The grammars, constructor views, and S/E/N maps preserve this distinction.
There is no active `More` alias or name-conversion adapter.
The committed terminal answer is `Solo(Owners,state)` in S and `Solo(state)`
in E/N. `One` remains an uncommitted Search value; the current strict grammars
reject the older `Last` shell. `Yield` and `Emit` still retain an `Answer`
payload because its private introductions do not belong to the residual.
[constructor-tests.rkt](../constructor-tests.rkt) checks the accepted and
rejected forms, eager-tail contexts, and the unchanged commitment boundary.

| Module | Responsibility |
| --- | --- |
| `core/{s,e,n}/language.rkt` | Live primitive grammars, allocation operations, and unification/disequality kernels; byte-identical provenance is in [core/PROVENANCE.md](core/PROVENANCE.md) |
| `kernel.rkt` | Allocation, substitution, introduction-group and allocated-name support operations, native S/E/N atomic kernel outcomes |
| `kernel-equations.rkt` | S atomic equations instantiated with functional or data outcome constructors by the S reference derivation |
| `grammar-{s,e,n}.rkt`, `ownerless-grammar.rkt`, `feature-schema.rkt` | Search/control/frontier syntax and feature productions, without source contractions |
| `relation-grammar.rkt` | Relation definitions, calls, and program syntax extending the S/E/N grammars |
| `maps.rkt`, `wf.rkt` | Structural S/E/N maps and well-formedness predicates |
| `stages/schema.rkt`, `stages/views.rkt`, `stages/maps.rkt` | Generic decomposition/refocusing machinery, constructor views, and structural stage maps |
| `control-transform.rkt` | Syntactic tail-call transformation used by the machine generators |
| `runtime.rkt` | Budget validation and exhaustion data for machine drivers |

The `core/` providers are required by current execution and validation. Their
historical origin records provenance, not archival status; removing a retired
route does not make these shared dependencies disposable.

The shared grammars and stage views contain no syntactic `prefix` operation
or frame. Internal S force retains Owners on the running body; E/N force
enters the stored body directly. The S reference source and the native matrix
state their contractions independently over this common syntax and stage
construction. The functional interpreter's `prefix` helper only attaches
Owners to an already mature Search; it is not a pending source computation
or a continuation waiting for a resumed body.

`current-atomic-observer` in the kernel equations is an optional work-trace
hook. Execution does not use it to recover configurations, closures, or scope.
Test fixtures and reusable assertions live in [test-support/](../test-support/README.md).

[layout-tests.rkt](../layout-tests.rkt) checks the module dependency boundary:
S reference execution and derivation depend only on `s-reference/` and
shared modules within this derivation tree. Shared and test-support modules
cannot depend on experimental evaluators or their test suites. Sources and
tests principally about an alternative account belong to that experiment.
