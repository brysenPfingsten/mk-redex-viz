# Modeling-miniKanren-in-Redex

The visualizer offers a **Strict scheduler lattice** with No Interleave,
Flip-Flop, and Railroad, plus the **Strict reference (Flip)** view of the
[S/E/N research matrix](racket-server/derivations/matrix/README.md).
It defaults to Railroad. All choices use strict matrix reduction semantics
and retain their actual configurations in session history. Both operands and
bind residuals mature before commitment; only Delay suspends work.

## Project map

**Earlier dormant-branch semantics** is the label for the retained
`DisjL`/`DisjR` account: it leaves a sibling's goal work dormant while the
active branch proceeds. Its source-relative derivations remain useful
comparisons. The current GUI executes the strict matrix sources.

`src/` compiles, runs, presents, and serves a selected semantics.
`derivations/` contains the semantics and machines themselves. The current
strict account occupies its top level; each alternative account keeps its
source, derivation, and evidence together under `experiments/`.

```text
project/
|-- frontend/                         GUI controls and tree rendering
|-- contracts/                        Shared visible-node contract
|-- docs/                             Architecture and semantic boundaries
`-- racket-server/
    |-- src/
    |   |-- transpiler/               Source, association, Delay placement
    |   |-- search-runtime.rkt        Select strict matrix scheduler
    |   |-- program-runner.rkt        Steps, history, Back/Reset
    |   |-- search-picture.rkt        Scope, candidates, committed answers
    |   |-- minikanren.rkt            Automatic run/run* consumption
    |   `-- app.rkt                   HTTP interface
    |
    |-- derivations/
    |   |-- all.rkt                  Current strict account only
    |   |-- s-reference/             S interpreter, both derivations,
    |   |                            machine maps, registers, compression
    |   |-- matrix/
    |   |   |-- full-source.rkt      Full S/E/N source reductions
    |   |   |-- scheduler-source.rkt Strict GUI scheduling providers
    |   |   |-- stages/              D/Z/M/B data stages and maps
    |   |   `-- big/                 Finite judgments and certificates
    |   |-- shared/
    |   |   |-- core/{s,e,n}/        Live variable/grammar/kernel providers
    |   |   `-- stages/              Shared stage construction
    |   |-- test-support/            Witnesses, generators, shared assertions
    |   `-- experiments/
    |       |-- all.rkt              Both alternatives and architecture checks
    |       |-- dormant-branch-semantics/
    |       |   |-- source/          Languages, reductions, WF, inspection
    |       |   |-- derivation/      Interpreters, machines, correspondence maps
    |       |   `-- tests/           Source laws and strict/dormant comparisons
    |       `-- early-conjunction-distribution/
    |           |-- languages/      Alternative distribution contexts
    |           |-- reduction-relations/
    |           `-- tests.rkt        Comparison with factored conjunction
    `-- tests/                       Application and cross-account integration
```

Braces and grouped names abbreviate sibling directories; the tree selects
the main files within them. Shared providers are live dependencies even
though their source ancestry predates the current strict derivation.
The experiments change semantic choices; they are not compression stages.
Their placement keeps them executable without giving them authority over the
current strict account.

See the [strict research inventory](racket-server/derivations/README.md)
for individual artifacts and the [policy guide](docs/semantic-policy-matrix.md)
for the retained alternatives.

## Derivation and GUI connections

The S reference checkpoint connects two derivations of the same strict
operations and pending work:

```text
s-reference/interpreter.rkt                 s-reference/source.rkt
            |                                         |
           CPS                                    decomposition
            |                                         |
   data + defunctionalization                       refocusing
            |                                         |
   functional data machine <----------------> syntactic data machine
            |              configuration maps         |
            |              and prescribed spans       +--> structural
        registers                                          compression
            |
   atomic-handler compression
```

The functional compression replaces a particular three-dispatch atomic
sequence with one step; its continuation remains pending. The syntactic
route has its own structural compression. The displayed machine connection
does not identify the two compressed endpoints or establish a final
`kappa / Q / pi` machine. Configuration checks are evidence; universal
correspondence and productivity proofs remain open.

The native matrix carries the strict operations and allocation-scope invariant
across S/E/N and through R/D/Z/M/B/Big. The GUI scheduler extension reaches
the following sources:

```text
strict matrix reference (Flip): S / E / N, including relation programs
                     |
               select full S
                     |
         +-----------+------------+
         |           |            |
    No Interleave Flip-Flop    Railroad
    retain priority reference  retain orientation
         |           |            |
         +-----------+------------+
                     |
         matrix/scheduler-source.rkt
```

For No Interleave and Railroad, separate E/N scheduler rows and downstream
machine derivations remain open. Railroad has explicit orientation-erasure
checks against the strict Flip-Flop source. These schedulers preserve strict
operand order, eager Search tails and bind, and separate commitment.

The application selects those source relations and retains their actual
configurations in the session:

```text
mini/micro --> transpiler --> strict configuration
                  ^                  |
          compilation profile        v
matrix/scheduler-source.rkt --> search-runtime.rkt <-- scheduler selection
                                     |
                              program-runner.rkt
                                /           \
                      manual history     minikanren.rkt
                            |            automatic run/run*
                     picture + HTTP
                            |
                        frontend
```

Compilation association and Delay placement are independent of runtime
scheduling. Relation calls are also an independent extension; expansion
adds no implicit Delay. Guardedness conditions belong to claims about
productive recursive behavior, not to the location of a scheduler coordinate.

## **Docker Setup**

Follow the steps below to clone this repository, set up Docker, and run the application.

### **Prerequisites**
Before you begin, ensure you have the following installed:

- **[Docker](https://docs.docker.com/get-docker/)** – to run containers
- **[Docker Compose](https://docs.docker.com/compose/install/)** – to manage multi-container applications

---

## **Installation and Setup**

Open a terminal and run:
```sh
git clone https://github.com/brysenPfingsten/mk-redex-viz.git Modeling-miniKanren-in-Redex
cd Modeling-miniKanren-in-Redex
docker compose -f docker-compose.dev.yml up --build
```
Finally, visit [localhost:5173](http://localhost:5173).

### Docker Compose Notes

- `docker-compose.dev.yml` serves the frontend on host port `5173`; the backend servlet uses port `5000` inside the Compose network.
- In the dev frontend container, API calls are expected to go through the Vite proxy (`/api -> racket-server:5000`).
- `docker-compose.yaml` binds frontend on `8080`; if that port is in use, startup will fail with an "address already in use" error.

## **Test Lanes**

Use the [test-lane inventory](racket-server/tests/TEST-LANES.md) for focused
commands and evidence boundaries. Run from the repository root with the
installed Racket dependencies and an isolated compiled root:

```sh
export PLTCOMPILEDROOTS=/private/tmp/full-strict-checks:
```

Run the comprehensive backend gate and frontend checks:

```sh
racket -y -l raco -- test racket-server/tests/test-all-headless.rkt
npm --prefix frontend test
npm --prefix frontend run lint
npm --prefix frontend run build
```

The backend gate includes the current strict derivations, the separate
experiments, and application checks. Passing an experiment's tests establishes
only its stated source-relative relationship. Focused commands and recorded
results live in one [test-lane inventory](racket-server/tests/TEST-LANES.md):

| Focus | Detailed commands and evidence |
| --- | --- |
| App/API, rendering, source modes and twelve compiler profiles | [Compiler and application gates](racket-server/tests/TEST-LANES.md#compiler-manual-session-and-application-payloads) |
| Answer limits, surplus answers, step caps and host reification | [Automatic consumer and library](racket-server/tests/TEST-LANES.md#automatic-consumer-and-minikanren-library) |
| S reference, native S/E/N cells, stages and Big | [Strict derivation gates](racket-server/tests/TEST-LANES.md#strict-source-derivations-and-representation-matrix) |
| Earlier dormant-branch semantics and early conjunction distribution | [Experiment gates](racket-server/tests/TEST-LANES.md#experiments-and-dormant-branch-account) |
| Frontend, browser checks and completed validation checkpoints | [Aggregate status](racket-server/tests/TEST-LANES.md#frontend-and-aggregate-status) |

## **Backend Init Contract**

The GUI/API boundary selects each run structurally.

`POST /api/post/init` accepts:

- `text`
- `sourceMode` = `"mini"` or `"micro"`
- optional `compileProfile` when `sourceMode = "mini"`
- optional `searchStrategy`, either:
  - `{ "scheduler": "rail" }` for the strict scheduler lattice, with `"dfs"`, `"flip"`, or `"rail"`
  - `{ "model": "strict" }` for the existing strict reference (Flip)

Omitting the selection uses Strict Search at the API/library boundary. The GUI
defaults to Strict scheduler lattice and explicitly sends `{ "scheduler": "rail" }`.
The reference is not a fourth scheduler. Switching views
preserves the chosen lattice scheduler and source settings; runtime and
compilation controls freeze during execution.

`compileProfile` controls conjunction/disjunction association and explicit
delay placement independently of runtime scheduling. All selections share
compiled goals, relation definitions, HTML source IDs, and query metadata.
Every choice initializes the same strict configuration:

```text
(program Γ (commit (eval (Owners) query-goal initial-state)))
```

The backend checks the selected grammar and well-formedness and records its
named reductions. No running configuration is converted between schedulers.
Relation expansion adds no implicit Delay.

Payload status distinguishes `running`, `paused`, `complete`, and `stuck`.
At a paused `More(Delay(...))` Frontier, the next manual step records public
`advance` before its source contractions, for every scheduler. Internal
`force-delay` remains a reduction. Completed payloads retain Done/Solo. GUI stepping
ignores the source `run n` limit; automatic consumption is a library operation.

### Reading the picture

The strict S picture draws each introduction group **on its owning node**,
with the variables introduced together and their source identity. Clicking a
visible group's source selects its source expression. Empty groups remain
visible. Common introductions govern the owner's children; the head-private
introductions of `Yield` and `Emit` stay on their `Answer` payload. A terminal
`Solo` owns its introductions and state directly, without a separate Answer
child. The current view does not insert `Freshened` wrapper nodes.

Cards label Search values, pending operations, Frontier structure, committed
answers, and goal syntax separately. Pending operations have dashed borders;
Search values have rounded borders; Frontier and committed-answer cards have
a double left border. An eager `Yield` whose tail is still computing is a
pending operation. A dotted edge below `Delay` leads to suspended work, not
the active reduction path. The legend remains beside the drawing.

The toolbar offers **Reduction step** while running and **Advance past Delay**
at a public boundary. Advancement records the request; subsequent reductions
evaluate and commit its result. History position includes both kinds of
action, with separate reduction and public-advance counts. **Underlying
configuration** expands the exact backend term, including Owners and source
labels, alongside the picture. Back and Reset restore that term and its counts.

## **Direct Library Surface**

If you want to run programs without the site, import
`racket-server/src/minikanren.rkt` and call `run-source` or
`run-source->answers` directly.

```racket
#lang racket

(require (file "racket-server/src/minikanren.rkt"))

(run-source->answers
 "(defrel (same x y)
    (== x y))
  (run* (q)
    (same q 'cat))")
;; => '(#hasheq((sym . "cat")))
```

The automatic adapter accepts the same source and compilation settings as the app,
plus consumption limits:

- `#:source-mode` (`"mini"` or `"micro"`)
- `#:compile-profile` for mini source
- `#:search-strategy`: `(strict-search)` by default, or
  `(search-strategy "dfs")`, `(search-strategy "flip")`, `(search-strategy "rail")`
- `#:step-cap` to bound diverging programs
- `#:answer-limit` to stop at an exposed Delay with enough answers, or at completion

The adapter saves the selected family's native configuration. Automatic answer
limits live in `minikanren.rkt` as a driver policy. For a positive limit the
driver reaches the next exposed Delay or terminal Frontier before testing the count;
in Strict Search this finishes eager evaluation and the entire commitment.
It leaves the next exposed Delay unforced and also stops on completion with
fewer answers. A zero limit returns immediately without stepping.
For every scheduler this intentionally continues past committed answers;
an unguarded residual can prevent reaching the next Delay and exhaust the step cap.

Returned answer lists contain at most the requested number. The saved
configuration and its picture retain the whole Frontier, including any surplus
answers in the final round. Manual session stepping, including the GUI, does
not enforce an answer limit. Source `run n` metadata does not limit that manual
execution; the Racket `run` binding below passes `n` to the automatic driver.

`minikanren.rkt` also provides `defrel`, `run`, and `run*` bindings for the mini
surface syntax, backed by this project's modeled Redex semantics.

`run*` runs the modeled search to completion and returns reified answers.
`run n ...` returns the first `n` answers after finishing the first round that
has accumulated enough. It leaves the next exposed Delay unforced and never
stops partway through commitment.

```racket
#lang racket

(require (file "racket-server/src/minikanren.rkt"))

(defrel (same x y)
  (== x y))

(run* (q)
  (same q 'cat))
;; => '(cat)

(run 2 (q)
  (conde
    [(== q 'a)]
    [(== q 'b)]
    [(== q 'c)]))
;; => '(a b)
```

The module-level bindings have one limitation:

- relation definitions are tracked per file/module, so keep the `defrel`s and
  the corresponding `run`/`run*` in the same source file unless you use an
  explicit evaluator object

For initialization, individual steps, and history without automatic consumption,
use `racket-server/src/program-runner.rkt`: `open-source` or `open-forms`, followed
by `model-session-step`, `model-session-back`, or `model-session-reset`.
`minikanren.rkt` also re-exports this session API; its automatic driver uses the
same public operations as the GUI.

## **Semantics Reading Order**

If you are studying the repo as a semantics artifact, use this order:

1. [Semantics organization](docs/semantics-ladder.md): independent compiler,
   representation, feature and derivation-stage choices; application flow.
2. [Strict derivation guide](racket-server/derivations/README.md) and
   [correction log](racket-server/derivations/CORRECTIONS.md):
   current inventory and why its semantic boundaries matter.
3. [S reference](racket-server/derivations/s-reference/README.md):
   source/interpreter, CPS, defunctionalization, machine maps, registers and
   prescribed compression spans.
4. [S/E/N matrix](racket-server/derivations/matrix/README.md):
   native feature and full relation cells, allocation maps, data stages and Big.
5. [Policy boundary](docs/semantic-policy-matrix.md) and
   [experiments](racket-server/derivations/experiments/README.md):
   retained alternatives and their distinct observations.

## **Current Runtime Surface**

The default GUI executes strict Railroad from
[`matrix/scheduler-source.rkt`](racket-server/derivations/matrix/scheduler-source.rkt).
The separate Strict reference (Flip) view executes the same full S source as
Flip-Flop in
[`matrix/full-source.rkt`](racket-server/derivations/matrix/full-source.rkt).
The [matrix guide](racket-server/derivations/matrix/README.md#files-and-interfaces)
owns the scheduler equations, operand orientation, public/internal forcing
distinction, and Railroad-to-Flip-Flop map. Its
[allocation contract](racket-server/derivations/matrix/README.md#allocation-representations)
explains common and answer-private introduction groups, including empty and
unused groups. Numeric-looking variable labels do not change the GUI's S
representation; there is no S/E/N GUI selector.

The [research inventory](racket-server/derivations/README.md#sen-coordinate-inventory)
records the native S/E/N stages and full relation-program correspondence.
Separate E/N scheduler rows, downstream scheduler machines, and universal
proofs remain open. The GUI still executes source reductions.

The [experiment guides](racket-server/derivations/experiments/README.md)
own the earlier dormant-branch derivations and the early conjunction
distribution counterexample. They preserve their grammars, raw-rule
interfaces, tests, and correspondence limits together. Neither experiment
provides the GUI runtime or constitutes a strict machine-compression stage.

## **Orientation (Minimal)**

Use this if you are jumping in with no project history:

| Location | Responsibility |
| --- | --- |
| [src/transpiler/](racket-server/src/transpiler/) | Parse mini/micro, apply compilation profile, preserve source IDs, initialize the strict program configuration |
| [experiments/dormant-branch-semantics/](racket-server/derivations/experiments/dormant-branch-semantics/README.md) | Complete earlier account: sources, interpreter/machine derivation, and comparison tests |
| [matrix/full-source.rkt](racket-server/derivations/matrix/full-source.rkt) | Native full S/E/N reduction relations |
| [matrix/scheduler-source.rkt](racket-server/derivations/matrix/scheduler-source.rkt) | Strict S scheduler variations and native Railroad orientation used by the GUI |
| [shared/wf.rkt](racket-server/derivations/shared/wf.rkt) | Strict representation-specific scope, store and relation checks |
| [src/search-runtime.rkt](racket-server/src/search-runtime.rkt) | Select strict scheduler relations and WF; inspect status and public boundaries |
| [src/program-runner.rkt](racket-server/src/program-runner.rkt) | Manual session, exact configuration history and back/reset |
| [src/app.rkt](racket-server/src/app.rkt) | HTTP/API boundary and source conversion |
| [src/search-picture.rkt](racket-server/src/search-picture.rkt) | Project strict scheduler terms with branch orientation, owner annotations, candidates and committed answers |
| [src/search-picture-common.rkt](racket-server/src/search-picture-common.rkt) | Shared logical-state, Owner and goal drawing; dormant-tree control inspection belongs to its experiment |
| [src/minikanren.rkt](racket-server/src/minikanren.rkt) | Automatic consumer and run/run* library interfaces |
| [Frontend examples](frontend/src/utils/example_programs.js) | Source-of-truth example programs, read by compiler and integration tests |

Focused source-mode and profile checks are listed with the
[compiler and application gates](racket-server/tests/TEST-LANES.md#compiler-manual-session-and-application-payloads).

## **Configuration**

The checked-in Dockerfiles and Compose files do not pin an architecture.
The backend uses `racket/racket:latest`; the architecture and behavior therefore
depend on the resolved image and local Docker configuration. The Apple Silicon
note below records a historical emulation issue, not a current build guarantee.

## **Issues**

### `Error reading from ~a`

An earlier Apple Silicon Docker run reported:

```
Error: error reading from ~a
("petite")
Aborted
```


The recorded reproducer was (the `latest` image was not pinned):

```
$ docker run -it --platform linux/amd64 racket/racket:latest sh -c "uname -m; racket"
x86_64
Error: error reading from ~a
("petite")
Aborted
```

The reported workaround was to disable `Use Rosetta for x86_64/amd64 emulation
on Apple Silicon` and select QEMU as the VMM. The recorded menu path was
Docker.app > Settings > General > Virtual Machine Options. That path and
workaround have not been revalidated for current Docker Desktop releases.
