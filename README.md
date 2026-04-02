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

### **3) Legacy semantics lane (manual host lane)**

This lane exercises legacy Redex models and visual smoke tests:

```sh
raco test \
  racket-server/tests/test-reduction-relations.rkt \
  racket-server/tests/unit-tests.rkt \
  racket-server/tests/translator-tests.rkt \
  racket-server/tests/visual-tests.rkt \
  racket-server/tests/test-dmitry-and-dmitry.rkt
```

Optional interactive visual check:

```sh
racket racket-server/tests/visual-tests.rkt
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
