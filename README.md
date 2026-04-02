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

Includes syntax-compat checks that frontend example programs parse and lift to `L4` target syntax.

### **2) App/API regression lane**

```sh
raco test racket-server/tests/test-all.rkt
```

### **3) Frontend compatibility-gating lane**

```sh
npm --prefix frontend test
```

### **4) Model×Example API-flow matrix lane**

Automates model selection + example execution checks across the full cross-product
using backend endpoints (analyze/model/init/step), up to 25 steps or termination.

```sh
raco test racket-server/tests/model-example-matrix-tests.rkt
```

## **Backend Model Registry**

The backend now exposes available stepping models through:

```text
GET /api/get/models
```

Each entry includes:
- `id` (used by `POST /api/post/model`)
- `label` (display name)
- `parserProfile` (currently `"surface->l4"` for all registered models)
- `parserTarget` (currently `"L4/config"` for all registered models)

## **LLM Orientation (Minimal)**

Use this if you are jumping in with no project history:

- Canonical parser/transpiler target is **L4 config syntax**:
  - `parserProfile = "surface->l4"`
  - `parserTarget = "L4/config"`
- Backend canonical entry points live in:
  - `racket-server/src/transpiler.rkt` (`parse-prog/canonical`)
  - `racket-server/src/app.rkt` (`init!` enforces canonical config shape)
  - `racket-server/src/model-registry.rkt` (exposes parser contract in `/api/get/models`)
- Canonical WF stack is split by layer:
  - `racket-server/src/wf-kernel.rkt` (shared term/state/substitution checks)
  - `racket-server/src/wf-core.rkt` (core judgments/shapes)
  - `racket-server/src/wf-variants.rkt` (L1/L2/L3/L4 judgments)
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
