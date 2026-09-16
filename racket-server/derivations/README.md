# Strict Search research artifact

The current account is the **S reference** for strict **Search**. Its
[interpreter](s-reference/interpreter.rkt) and
[reduction semantics](s-reference/source.rkt) have functional and syntactic
derivations connected by a machine-configuration map and finite transition
checks, followed by registers and one bounded atomic-outcome compression.
The [S/E/N matrix](matrix/README.md) carries the same strict operations and
allocation-scope invariant through R/D/Z/M/B/Big in twelve native feature cells.
Its Search S row is checked against this checkpoint, and configuration-level transition
checks connect the checkpoint machine to the native E/N machines.

The [full relation-program extension](matrix/full-source.rkt) adds explicit Γ
environments, named calls, recursive calls, and mutual recursion in all three
rows. It is the source relation used by the GUI's separate **Strict reference (Flip)**
view and the default API/library selection.
The S reference functional derivation carries the same environment explicitly
through its data machines, generated registers, and existing compression.

The GUI defaults to **Strict scheduler lattice / Railroad**, with No Interleave,
Flip-Flop, and Railroad using the [strict S scheduler extension](matrix/scheduler-source.rkt).
Flip-Flop reuses the reference relation; No Interleave changes delayed-merge
priority, and Railroad adds `mplusR` and eager `YieldR` with a checked orientation map.
Separate No Interleave/Railroad E/N rows and downstream scheduler derivations remain open.
The historical phrase “Search/rail” names this derivation's strict Search
feature, rather than either Railroad extension. The earlier `DisjL`/`DisjR`
sources are retained as the **earlier dormant-branch semantics**.

The S reference interpreter and its matrix counterparts preserve strict
left-to-right disjunction, eager `Yield` tails and bind, exact allocation
ancestry, and the Search/Frontier commitment boundary. Only object-language
`Delay` suspends computation:

```text
Search   ::= Empty(O) | One(O,σ) | Yield(O,A,Search) | Delay(O,R)
Frontier ::= Done(O) | Solo(O,σ) | Emit(O,A,Frontier)
           | Forced(O,Frontier) | More(Delay(O,R))
```

These are the S shapes; E/N carry their own allocation information.
`Yield` contains an active candidate and an eager Search tail. Unary `More`
holds unfinished Frontier work. `Done` and `Solo` retain distinct completion
structure. Observations compare exact Frontiers, including suspended bodies.
`Solo(O,σ)` owns its terminal state and introductions directly. `Answer(O,σ)`
remains a separate head payload in `Yield` and `Emit`, where its private
introductions must be distinguished from the owners shared with the tail.

## Reading order and maintained layout

The directories here define the current strict account. Application plumbing
under [`src/`](../src/) selects and presents these semantics; being live in
the GUI does not move a calculus into application infrastructure. Alternative
accounts and their own evidence live together under
[`experiments/`](experiments/README.md).

1. Read the [correction log](CORRECTIONS.md) and its small witnesses.
2. Inspect [representations and primitives](shared/README.md), then the
   [interpreter](s-reference/interpreter.rkt) and
   [source equations](s-reference/source.rkt).
3. Follow the [derivation stages](s-reference/README.md),
   [machine correspondence](s-reference/CORRESPONDENCE.md), and
   [register/compression contracts](s-reference/REGISTERIZATION.md).
4. Use the coordinate inventory below to read the
   [matrix source specification](matrix/README.md),
   [native stages](matrix/stages/README.md), and
   [Big equations and certificates](matrix/big/README.md).

| Location | Responsibility |
| --- | --- |
| [s-reference/](s-reference/README.md) | S reference interpreter, source, functional/syntactic correspondence, registers, and first compression |
| [shared/](shared/README.md) | S/E/N variable/state languages, allocation and kernels, grammars, structural maps, well-formedness, and narrow transformation machinery |
| [matrix/](matrix/README.md) | Native strict S/E/N feature instances through R/D/Z/M/B/Big and their connection to the S reference checkpoint |
| [test-support/](test-support/README.md) | Named witnesses, generated lexical goals, and neutral random/structural/transition helpers shared by validation suites |
| [all.rkt](all.rkt) | Current strict aggregate: S reference, matrix, constructor contracts, and dependency/layout checks |
| [experiments/](experiments/README.md) | Complete alternative accounts, with a separate aggregate for their sources, derivations, and comparisons |

The [dormant-branch account](experiments/dormant-branch-semantics/README.md)
and [early conjunction distribution](experiments/early-conjunction-distribution/README.md)
are semantic alternatives. Their separate gates preserve positive maps and
counterexamples; they are outside this matrix and derivation pipeline.

## S reference derivation and evidence

```text
interpreter → CPS → data + defunc → functional machine → registers → compressed
                                        ↕ configuration correspondence
source → decomposition D → refocusing Z → native M → native B
```

Native B removes structural navigation around one source contraction.
Functional compression combines an atomic evaluation with its two
outcome-handler dispatches; these are distinct downstream transformations.

| Artifact | Implementation and checks | Remaining obligation |
| --- | --- | --- |
| Interpreter and CPS | [interpreter.rkt](s-reference/interpreter.rkt), [cps.rkt](s-reference/cps.rkt); [interpreter tests](s-reference/interpreter-tests.rkt) | General direct/CPS correctness over the admitted domain |
| Defunctionalized program and machine | [data.rkt](s-reference/data.rkt), [defunc.rkt](s-reference/defunc.rkt), [machine.rkt](s-reference/machine.rkt); [data/machine tests](s-reference/defunc-tests.rkt) | General defunctionalization and generation correctness |
| Source and syntactic stages | [source.rkt](s-reference/source.rkt), [stages.rkt](s-reference/stages.rkt); [source tests](s-reference/source-tests.rkt) | Domain preservation and general R/D/Z/M/B correspondence |
| Machine relation | [configuration map](s-reference/machine-correspondence.rkt), independent [readback](s-reference/readback.rkt), [administrative rank](s-reference/administration.rkt); [transition checks](s-reference/machine-correspondence-tests.rkt) | All-configuration labelled diagrams and native administrative progress; [contract](s-reference/CORRESPONDENCE.md) |
| Registers and compression | [registers.rkt](s-reference/registers.rkt), [compressed.rkt](s-reference/compressed.rkt), [span maps](s-reference/compression-correspondence.rkt); [register tests](s-reference/register-tests.rkt), [span tests](s-reference/register-compression-tests.rkt) | Generator/mutation correctness and prescribed spans on the full domain; [contract](s-reference/REGISTERIZATION.md) |
| S/E/N correspondence | [source maps](shared/maps.rkt), [stage maps](shared/stages/maps.rkt), [domain predicates](shared/wf.rkt); [checkpoint transition checks](matrix/s-reference-tests.rkt) | Universal source/stage diagrams and domain preservation; E/N functional interpreters and register programs are not separately derived |

The interpreters do not execute the source relation. The functional machine
map constructs native controls and continuation fields directly. Independent
whole-source readback checks that map; completed-answer equality alone is
not the correspondence criterion.

## S/E/N coordinate inventory

S uses named variables with ordered tagged introduction groups along the active
scope. E uses named variables with ordered state-local allocated-name Support.
N uses positional variables with a state-local allocation counter. The live
[primitive providers](shared/core/PROVENANCE.md) retain each representation's
unification and disequality kernels. [Kernel checks](matrix/kernel-tests.rkt)
exercise their native outcomes and state preservation.

Every cell below has native R/D/Z/M/B and Big implementations of the same
strict operations. The [matrix source contract](matrix/README.md#program-active-search-and-settled-frontier)
specifies retained scope during internal force, commitment, and public
advancement; its [allocation representations](matrix/README.md#allocation-representations)
explain the S introduction groups, E support, and N supply.

| Feature | S source | E source | N source | Correspondence checks |
| --- | --- | --- | --- | --- |
| Core | [StrictSCore](matrix/features.rkt) | [StrictECore](matrix/features.rkt) | [StrictNCore](matrix/features.rkt) | [features](matrix/feature-tests.rkt), [stages](matrix/stages/tests.rkt), [Big](matrix/big/tests.rkt) |
| Delay | [StrictSDelay](matrix/features.rkt) | [StrictEDelay](matrix/features.rkt) | [StrictNDelay](matrix/features.rkt) | [features](matrix/feature-tests.rkt), [stages](matrix/stages/tests.rkt), [Big](matrix/big/tests.rkt) |
| Disjunction | [StrictSDisjunction](matrix/features.rkt) | [StrictEDisjunction](matrix/features.rkt) | [StrictNDisjunction](matrix/features.rkt) | [features](matrix/feature-tests.rkt), [stages](matrix/stages/tests.rkt), [Big](matrix/big/tests.rkt) |
| Search | [S](matrix/source-s.rkt) | [E](matrix/source-e.rkt) | [N](matrix/source-n.rkt) | [source maps](matrix/tests.rkt), [generated goals](matrix/property-tests.rkt), [stages](matrix/stages/tests.rkt), [Big](matrix/big/tests.rkt) |
| Search + relations | [StrictSRel](matrix/full-source.rkt) | [StrictERel](matrix/full-source.rkt) | [StrictNRel](matrix/full-source.rkt) | [source/stage maps](matrix/full-tests.rkt), [functional machine](s-reference/relation-tests.rkt), [Big](matrix/big/full-tests.rkt) |

[Stage instances](matrix/stages/instances.rkt) instantiate all twelve cells.
Big uses [S](matrix/big/s.rkt), [E](matrix/big/e.rkt), and [N](matrix/big/n.rkt)
for Search and [features.rkt](matrix/big/features.rkt) for the other nine.
[Big maps](matrix/big/maps.rkt) map every recursive certificate node; target
proofs are obtained independently. The
[source commitment checks](matrix/commit-source-tests.rkt) and
[stage commitment checks](matrix/stages/commit-tests.rkt) cover exact public
boundaries and direct S→E, E→N, and S→N squares.

The [checkpoint gate](matrix/s-reference-tests.rkt) compares the independently
stated S sources and native stages, then checks the S reference functional
machine's mapped configurations against actual S/E/N machine transitions.
These are prescribed source-operation and administrative spans, not only
readback equalities. Big independently supplies finite judgments and mapped
certificates for the aligned source rows. General adequacy, preservation,
machine correspondence, and productivity proofs remain open.

## Generated artifacts and reproduction

The S reference has three checked-in generated programs:

| Generated file | Generator and input |
| --- | --- |
| [machine.rkt](s-reference/machine.rkt) | [derive.rkt](s-reference/derive.rkt) reifies the `/d` bodies of [defunc.rkt](s-reference/defunc.rkt) |
| [registers.rkt](s-reference/registers.rkt) | [register-derive.rkt](s-reference/register-derive.rkt) transforms the same control bodies into PC/register dispatch |
| [compressed.rkt](s-reference/compressed.rkt) | [compression-derive.rkt](s-reference/compression-derive.rkt) checks the atomic-handler rewrite, then uses the register generator |

Each generator accepts `--check` for freshness; omit it to regenerate.
The S reference and overall aggregates also check freshness. Feature/source,
stage, and Big macros construct literal instances at module expansion without
additional checked-in generated programs. The
[tail-call transformer](shared/control-transform.rkt) and
[stage schema](shared/stages/schema.rkt) expose specific transformations;
they are not a universal semantic framework.

Run from the repository root:

```sh
racket racket-server/derivations/s-reference/derive.rkt --check
racket racket-server/derivations/s-reference/register-derive.rkt --check
racket racket-server/derivations/s-reference/compression-derive.rkt --check
raco test racket-server/derivations/s-reference/all.rkt
raco test racket-server/derivations/matrix/all.rkt
raco test racket-server/derivations/all.rkt
racket racket-server/derivations/s-reference/show.rkt
racket racket-server/derivations/s-reference/show-machines.rkt
racket racket-server/derivations/s-reference/show-register-compression.rkt
```

The aggregate includes [constructor contracts](constructor-tests.rkt) and
[dependency boundaries](layout-tests.rkt). The narrow
[strict versus dormant-branch work-order witness](experiments/dormant-branch-semantics/tests/strict-policy-tests.rkt)
belongs to the dormant experiment's gate. Removed comparison suites are not
evidence for the maintained artifact; the
[correction log](CORRECTIONS.md#retired-and-deferred-results) records their
deliberately deferred unique results.

The maintained aggregate excludes retired numeric-interpreter/register
comparisons and their additional host-language domains. Native S/E/N
[work checks](matrix/work-tests.rkt) compare actual atomic work directly;
literal ownership, settled-prefix persistence, and independent register-bank
assertions remain in the S reference account. These do not retain the older
numeric Big proof-search or productive host-recursion results. Application
checks and their separate scope are described in
[TEST-LANES.md](../tests/TEST-LANES.md).

### Validation scope

The [test lanes](../tests/TEST-LANES.md) distinguish the current strict gate,
the experiment gate, and the comprehensive application gate. Recorded counts
describe completed validation checkpoints; they are not a claim that a later
edit has rerun those gates. The relation corpus includes fresh allocation
after a public Delay resumes, bounded productive prefixes, and unguarded
recursive runs. These finite checks do not establish universal correspondence,
preservation, or productive-stream theorems.

## Next correspondence and application boundary

Hold the S reference Search behavior and machine fixed while turning the
checked S/E/N configuration diagrams into general correspondence and domain
preservation arguments. The native E/N machines connect to the S reference
functional machine; separate E/N direct/CPS/defunctionalized/register
programs remain to be derived. Full relation programs now have exact finite
and bounded configuration checks on both sides, including explicit environments
and pending calls in resumptions. General recursive-program adequacy,
productive infinite behavior, and compact κ/Q/π rail compression remain open.

The GUI executes the full strict S source or its scheduler extension; it does
not execute a registerized or compressed machine. The matrix guide owns the
[scheduler equations and orientation map](matrix/README.md#files-and-interfaces).
The root README owns the [application flow](../../README.md#derivation-and-gui-connections),
[initialization and manual advancement](../../README.md#backend-init-contract),
and [automatic run/run* policy](../../README.md#direct-library-surface).
These consumers preserve actual source configurations and the same commitment
boundary. Separate E/N No Interleave/Railroad rows, downstream scheduler
derivations, and general correspondence proofs remain open; the
[semantic policy matrix](../../docs/semantic-policy-matrix.md) records those boundaries.
