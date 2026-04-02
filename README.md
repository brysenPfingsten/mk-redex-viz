# Modeling-miniKanren-in-Redex
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
git clone https://github.com/brysenPfingsten/Modeling-miniKanren-in-Redex.git
cd Modeling-miniKanren-in-Redex
docker login
docker compose -f docker-compose.dev.yml up --build
```
Finally, visit [localhost:5173](http://localhost:5173).

### Docker Compose Notes

- `docker-compose.dev.yml` is the supported dev stack (`frontend` on `5173`, backend servlet on `5000`).
- In the dev frontend container, API calls are expected to go through the Vite proxy (`/api -> racket-server:5000`).
- `docker-compose.yaml` binds frontend on `8080`; if that port is in use, startup will fail with an "address already in use" error.

## **Test Lanes**

Use the lane that matches what you are validating.

### **1) Headless lane (default CI/local smoke)**

```sh
raco test racket-server/tests/test-all-headless.rkt
```

Includes the modern runtime surface only:
- helper/property checks
- search-lattice/internal runtime tests
- frontend example compatibility
- structured strategy overlap/confidence/matrix checks

### **2) App/API regression lane**

```sh
raco test racket-server/tests/test-all.rkt
```

### **3) Frontend lane**

```sh
npm --prefix frontend test
```

### **4) Strategy×Example API-flow matrix lane**

Automates structured search-strategy selection + example execution checks across
the surfaced cross-product using backend endpoints, up to 25 steps or
termination.

```sh
raco test racket-server/tests/model-example-matrix-tests.rkt
```

## **Backend Init Contract**

The GUI/API boundary no longer exposes raw backend model ids. A run is selected
structurally instead.

`POST /api/post/init` accepts:
- `text`
- `sourceMode` = `"mini"` or `"micro"`
- optional `compileProfile` when `sourceMode = "mini"`
- `searchStrategy`, a JSON object with:
  - `hoist` = `"early"` or `"late"`
  - `scheduler` = `"dfs"`, `"flip"`, or `"rail"`

Default surfaced strategy:
- `hoist = "early"`
- `scheduler = "rail"`

Execution notes:
- `compileProfile` controls source-to-micro compilation choices such as
  conjunction associativity, disjunction associativity, and delay placement.
- `searchStrategy` controls the backend stepping machine independently of the
  source compilation settings.
- The backend parses directly to the canonical search-lattice target config and
  then steps that program under the internal `+calls` configuration selected by
  `searchStrategy`.

## **Semantics Organization**

The repo now has one authoritative runtime path:

- active search lattice:
  - languages: `racket-server/src/search-lattice/languages/*.rkt`
  - well-formedness: `racket-server/src/search-lattice/wf/*.rkt`
  - reducers: `racket-server/src/search-lattice/reduction-relations/*.rkt`
  - strategy registry: `racket-server/src/search-runtime.rkt`
  - structured strategy API: `racket-server/src/search-strategy.rkt`

The short architecture note lives in:

- `docs/semantics-ladder.md`

## **LLM Orientation (Minimal)**

Use this if you are jumping in with no project history:

- Canonical parser/transpiler target is the neutral search target:
  - `parserProfile = "surface->canonical"`
  - `parserTarget = "canonical/config"`
- Backend canonical entry points live in:
  - `racket-server/src/transpiler.rkt` (`parse-prog/canonical`)
  - `racket-server/src/app.rkt` (`init!` validates canonical shape, then checks the internal search target selected by `searchStrategy`)
  - `racket-server/src/search-runtime.rkt` (strategy registry, stepper lookup, internal wf checks)
  - `racket-server/src/search-strategy.rkt` (structured surfaced strategy contract)
- Canonical WF/target checks now live in the search-lattice side:
  - `racket-server/src/search-lattice/languages/canonical-core-lang.rkt`
  - `racket-server/src/search-lattice/languages/canonical-lang.rkt`
  - `racket-server/src/search-lattice/wf/canonical-core-wf.rkt`
  - `racket-server/src/search-lattice/wf/all.rkt` (canonical target registry + search-lattice wf exports)
- Internal search-lattice WF for the GUI/API boundary lives in:
  - `racket-server/src/search-lattice/wf/*.rkt`
- Frontend examples are source-of-truth in:
  - `frontend/src/utils/example_programs.js`
- Integration test auto-loads all frontend examples and checks parse + lift to canonical target:
  - `racket-server/tests/example-compat-tests.rkt`

Fast validation command:

```sh
raco test racket-server/tests/test-all-headless.rkt
```

## **Configuration**

The Docker images expect an amd64 platform. Users on Apple Silicon or other arm64 based architectures,
will need to rely on emulation. This build is known to build and works under QEMU.

## **Issues**

### `Error reading from ~a`

When building with Docker on an Apple Silicon machine, some users encounter an error like the following:

```
Error: error reading from ~a
("petite")
Aborted
```


Here is a minimal test that should produce the same error:

```
$ docker run -it --platform linux/amd64 racket/racket:latest sh -c "uname -m; racket"
x86_64
Error: error reading from ~a
("petite")
Aborted
```

To resolve this, open Docker.app and under Settings > General >
Virtual Machine Options, make sure you have un-checked `Use Rosetta
for x86_64/amd64 emulation on Apple Silicon`, and have selected QEMU as the VMM.
