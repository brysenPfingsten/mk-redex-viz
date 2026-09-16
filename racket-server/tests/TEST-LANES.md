# Test lanes

Run commands from the repository root, using the installed Racket dependencies.
An isolated compiled root avoids mixing cached artifacts from concurrent edits:

```sh
export PLTCOMPILEDROOTS=/private/tmp/full-strict-checks:
```

The trailing colon retains the normal compiled-root fallback. The gate layout
below separates the current strict account from alternative experiments.
The status table records completed relocation gates and the remaining browser
interaction check. Earlier counts are labelled as historical checkpoints.

## Strict source, derivations and representation matrix

```sh
racket -y -l raco -- test racket-server/derivations/all.rkt
```

This aggregate includes the S reference derivation, the native
S/E/N matrix, full relation-program checks, generated-artifact freshness, and
constructor/dependency contracts. It excludes experimental accounts and
strict/dormant comparisons; those run in the experiments gate.

Focused gates:

```sh
racket -y -l raco -- test racket-server/derivations/s-reference/all.rkt
racket -y -l raco -- test racket-server/derivations/matrix/all.rkt
racket -y -l raco -- test racket-server/derivations/matrix/s-reference-tests.rkt
racket -y -l raco -- test racket-server/derivations/matrix/full-tests.rkt
racket -y -l raco -- test racket-server/derivations/s-reference/relation-tests.rkt
racket -y -l raco -- test racket-server/derivations/matrix/big/full-tests.rkt
```

| Gate | What it checks |
| --- | --- |
| S reference | Direct/CPS/data machines, functional-to-syntactic configuration maps, register decoders, and prescribed compression spans |
| Matrix aggregate | Twelve call-free S/E/N feature cells through R/D/Z/M/B/Big, direct representation maps, generated goals, exact work and allocation scope |
| S reference checkpoint | Independently stated reference/matrix S sources and stages, and the reference functional machine mapped to actual native S/E/N transitions |
| Full source | Three additional relation cells, explicit `(program Γ q)`, native stage/configuration maps, calls, recursion and public boundaries |
| Functional relation extension | Explicit Γ captures and program frames through direct/CPS/data/register/compressed stages; exact connection to native full-language machines |
| Full Big | Independent finite judgments, fixed-point results, source-label traces and direct certificate maps with Γ in recursive premises |
| Strict GUI schedulers | Existing Flip source reused directly; DFS Delay variation; native Railroad orientation with exact single-step maps, full scope witnesses, strict bind and calls |

These checks compare configurations and intermediate Frontiers, not just final
answers. Witnesses cover strict sibling work, eager bind, nested rails,
internal versus public forcing, fresh across Delay, unused introductions,
sparse ancestry and lexical shadowing. Productive recursion is checked only
for bounded prefixes; deliberately unguarded calls must not invent a Delay
or commit a pending candidate.

The [research inventory](../derivations/README.md) gives generator
commands and proof obligations. Universal correspondence, domain preservation,
productive streams and further `κ / Q / π` compression remain open. No separate
E/N functional interpreter or register derivation is implied by the matrix.

## Experiments and dormant-branch account

```sh
racket -y -l raco -- test racket-server/derivations/experiments/all.rkt
raco test racket-server/derivations/experiments/dormant-branch-semantics/all.rkt
racket racket-server/derivations/experiments/dormant-branch-semantics/derivation/derive.rkt --check
racket racket-server/derivations/experiments/dormant-branch-semantics/derivation/show.rkt
```

The [experiment overview](../derivations/experiments/README.md) describes the
two accounts and their separate gate. The
[dormant-branch guide](../derivations/experiments/dormant-branch-semantics/README.md)
owns that complete account: source languages/reductions/WF, interpreter and
machine derivations, structural maps, source laws, picture checks, and
strict/dormant comparison witnesses. It states the exact correspondence domains.
Direct/CPS/defunctionalized/generated machines agree with their demand source
through structural readback and prescribed 0/1 spans. The
source/native map checks exact configuration transitions on the allocation-free
empty-Owners fragment, and records an ownership-transport counterexample
outside it. Local one-step allocation/kernel fixtures exercise additional
labels without enlarging that full-trace claim. Native Railroad/Flip tests
cover the full grammar, all native rules, active work, fresh and relation calls
under the explicit orientation map. Strict work/commit differences, compiled
divergent loops, and scoped provenance observations remain separate gates.

The experiments aggregate also includes early conjunction distribution and
cross-experiment architecture checks. It is included in the headless gate.
Its diagnostics
name the earlier dormant-branch semantics explicitly; the current GUI uses the strict
matrix scheduler rows instead.

## Compiler, manual session and application payloads

```sh
racket -y -l raco -- test racket-server/tests/test-transpiler.rkt racket-server/tests/example-compat-tests.rkt
racket -y -l raco -- test racket-server/tests/search-runtime-tests.rkt racket-server/tests/model-example-matrix-tests.rkt
racket -y -l raco -- test racket-server/tests/scheduler-integration-tests.rkt
racket -y -l raco -- test racket-server/tests/test-app.rkt racket-server/tests/visible-contract-tests.rkt racket-server/tests/search-picture-tests.rkt
racket -y -l raco -- test racket-server/tests/frontier-example-tests.rkt racket-server/tests/confidence-gates-tests.rkt racket-server/derivations/test-support/runtime-test-support.rkt
racket -y racket-server/tests/ui-payload-smoke.rkt
```

`example-compat-tests` consumes every frontend example and checks both mini and
rendered micro against the full strict grammar, WF and explicit query metadata.
`model-example-matrix-tests` checks all twelve **compiler profiles** on a finite
relation program: 2 conjunction associations × 2 disjunction associations ×
3 delay placements. These are not the matrix's twelve representation/feature
cells. Direct sessions and HTTP-handler payloads are compared at every named
strict S source edge, including explicit public advancement, and the checks
are repeated on rendered micro. This gate exercises handlers without a live
network server.

The compiler's source-attribution cases check occurrence IDs and emitted spans
across all twelve profiles, including nested `conde`, reassociated conjunction,
repeated identical calls, shadowing, allocation, and explicit versus inserted
Delay. Runtime copies of a definition retain its source identity. The 2026-09-07
repair also compared 168 compiled configurations with their saved pre-change
targets, equal after erasing labels; seven compiler/display/JavaScript-parser
round trips checked literal escaping. These checks concern source attribution,
not a new semantic transformation.

The application gates distinguish paused More from completed Done/Last,
Search candidates from committed answers, and internal force from public
advance. All application sessions retain strict `(program Γ q)` configurations.
The dormant source tests initialize their own `(Γ F)` fixtures and select
their native relations directly. Their work-tree inspection lives in the
experiment's `source/inspection.rkt`; production `search-picture.rkt` accepts
strict terms. Logical-state, Owner, and goal drawing is shared through
`src/search-picture-common.rkt`. The application gates check source/state highlighting, exact common/private scope,
back/replay/reset, bounded responsiveness, and the absence of extra kernel work
during status inspection or rendering. The visible-contract entry point remains
part of `scripts/run_ui_smoke.sh`. The payload smoke prints actual full program
configurations, operation labels, statuses and committed counts.

Manual sessions in `src/program-runner.rkt` and the GUI do not enforce a source
`run n` limit. Their status describes the computation. Automatic consumption
is tested separately below. The GUI defaults to the strict scheduler lattice/Railroad and
retains No Interleave and Flip-Flop; its separate Strict reference (Flip) view sends
`{ "model": "strict" }`. API/library calls default to `(strict-search)`.
An explicit `search-strategy` with scheduler `"dfs"`, `"flip"`, or `"rail"`
instead selects the corresponding strict S matrix scheduler. All selections
share the strict program wrapper. Public `advance` is explicit for each;
`force-delay` is always internal. Railroad retains its native orientation
through rendering and uses a checked erasure map for comparison to Flip.
The scheduler integration suite covers 36 profile/scheduler traces, retained
scope, exact work/commit order, pending bind, and guarded and unguarded
recursion. Its current subject is the live strict runtime; dormant-source
and strict/dormant comparison suites belong to the experiment.

## Automatic consumer and miniKanren library

```sh
racket -y -l raco -- test racket-server/tests/program-runner-tests.rkt racket-server/tests/minikanren-library-tests.rkt
```

The automatic `run-source`/`run-forms` driver belongs to `src/minikanren.rkt`,
alongside run/run* and evaluator/module APIs. Limit handling stops at the first
exposed Delay with enough answers, or at completion. For Strict Search this
must finish the current eager round and commitment. All three strict schedulers
also check that an unguarded operand prevents premature answer commitment
and exhausts the step cap.
Returned answers can be a requested prefix while the saved configuration and
picture retain surplus committed answers. Tests also cover zero limits, finite
completion, step caps, source modes and host-value reification.

## Early conjunction distribution

```sh
racket -y -l raco -- test racket-server/derivations/experiments/early-conjunction-distribution/tests.rkt
```

[early-conjunction-distribution/](../derivations/experiments/early-conjunction-distribution/README.md)
sits beside the dormant-branch experiment and varies its source by distributing
conjunction over choice before machine derivation. Nested rails expose an observable
answer-order difference from the factored source. Its experiment-only raw
seam is local to `reduction-relations/factored-search-base.rkt`.

The experiments aggregate invokes this dedicated suite alongside the
dormant-branch account. Relocation does not add a strict
correspondence, GUI selector, matrix cell, or A7/A9 machine integration.
The alternative and its semantic assessment remain separate from the strict
application gates above.

## Frontend and aggregate status

```sh
npm --prefix frontend test
npm --prefix frontend run lint
npm --prefix frontend run build
```

Frontend tests cover runtime requests, the three strict lattice schedulers,
remembered settings, initialization/frozen controls, profile requests, source
mapping, state inspection, and the visible-node contract. Selector behavior
does not establish an interpreter correspondence.

`tests/test-all-headless.rkt` invokes `derivations/all.rkt` for the current
strict account, `derivations/experiments/all.rkt` for both alternatives and
their architecture checks, and the application/compiler/library/session/API
suites. Neutral generator and runtime-test helpers live in
`derivations/test-support/`; their location does not select an account.
Native source tests do not count as an interpreter correspondence proof.
The comprehensive entry point is:

```sh
racket -y -l raco -- test racket-server/tests/test-all-headless.rkt
```

HEADLESS raises on nonzero failures rather than silently succeeding.

The owner-annotation GUI checkpoint (2026-09-15) passed 3,765 backend tests,
including all 91 HEADLESS cases, and 60 frontend tests. The production build
passed; lint reported zero errors and the same three hook warnings.
Picture checks cover grouped and empty introductions, common/private scope,
source identity, eager-tail classification, and internal/public Delay
contractions. These checks establish the projection contract; visual
inspection is a separate check.

A fresh backend on port 5027 and frontend on port 5187 were also checked in
a real browser. `fresh branch disj` showed committed Frontier structure,
pending commitment, Search values, and their distinct owner annotations in
one intermediate picture. For `same`, No Interleave completed with four
answers after 44 reductions and three public advances; Flip-Flop, Railroad,
and the strict reference each took 42 reductions and three public advances.
The checks exercised the advancement button, exact configuration display,
owner-source highlighting, Back, and Reset. The test servers were separate
from any previously running browser/backend build.

Earlier checkpoints:

| Validation | Status |
| --- | --- |
| Relocated current strict aggregate | 3,294 tests passed |
| Relocated comprehensive headless gate | 3,760 tests passed, including all 90 HEADLESS cases; exit 0 |
| Relocated experiments aggregate | 343 tests passed |
| Layout and cross-experiment architecture checks | 9 tests each passed; included in their aggregates |
| Generated artifacts | All four freshness checks passed |
| Frontend | 56 tests and build passed; lint had zero errors and three unchanged hook warnings |
| `scripts/run_ui_smoke.sh` | Passed: 9 app cases, 4 visible-contract cases, and full payload checks |
| Isolated packaging | Copied application plus matrix/shared providers ran and rendered the strict reference and all three schedulers with exact answers; both Compose configurations validate |
| Relocated native browser check | `same` completed with four answers under No Interleave (47 steps), Flip-Flop (45), Railroad (45), and the strict reference (45); Back/replay/Reset passed; the reference console reported no errors |
| Pre-relocation headless checkpoint, 2026-09-07 | 3,721 tests passed, including 208 HEADLESS cases; overlapping counts, not additive |
| Pre-relocation browser check | All three strict schedulers and the reference view passed; see the [recorded application trace](../../docs/semantics-ladder.md#evidence-and-remaining-work) |
| Docker image/container execution | Unverified; the earlier daemon attempt returned HTTP 500 |

Focused counts overlap the aggregates and must not be added. The relocated
browser checks used a fresh isolated backend containing only application and
matrix/shared code, with the current frontend. Packaging keeps the imported matrix/shared
providers at their relocated paths and excludes the experimental providers.

`tests/test-all.rkt` is the GUI RackUnit runner, not the headless CI entry point.
Lattice operational suites are not substitutes for the strict source
and relation-stage checks. See the [policy boundary](../../docs/semantic-policy-matrix.md)
and [correction log](../derivations/CORRECTIONS.md).
