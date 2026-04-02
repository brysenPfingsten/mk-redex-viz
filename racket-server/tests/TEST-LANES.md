# Test Lanes

This directory has multiple intentional lanes. Keep them separate so failures are easier to triage.

## Lane A: Headless (default, modern runtime)

Runs deterministic/unit/property checks that do not require GUI interaction.

```sh
raco test racket-server/tests/test-all-headless.rkt
```

Includes:
- Core property/judgment checks
- Internal search-lattice tests
- Structured search-runtime registry + overlap audit
- Frontend example compatibility gate
- Structured strategy confidence/matrix checks

## Lane B: App/API Regression

Runs the app-level test suite used for server behavior regression checks.

```sh
raco test racket-server/tests/test-all.rkt
```

## Lane C: Frontend Unit Tests

Runs pure frontend unit tests.

```sh
npm --prefix frontend test
```

## Lane D: Strategy×Example API-Flow Matrix (automated GUI-proxy)

Runs full surfaced strategy/example stepping audit without manual clicking:
- init with selected search strategy (`POST /api/post/init`, payload includes `searchStrategy`)
- step up to 25 or termination (`GET /api/get/next`)
- assert payload shape each step (`step`, `stepName`, JSON `program`)

Coverage policy:
- Surfaced strategies only:
  - `hoist`: `early`, `late`
  - `scheduler`: `dfs`, `flip`, `rail`

```sh
raco test racket-server/tests/model-example-matrix-tests.rkt
```

## Lane E: Legacy Ladder Research Coverage

Runs the archived eager/lazy/proceed-era ladder suites. This lane is not part
of the default modern runtime gate.

```sh
raco test racket-server/tests/test-all-legacy.rkt
```

Implementation note:
- the root lane wrapper delegates to `racket-server/archive/legacy-ladder/tests/test-all-legacy.rkt`

## Notes

- Public GUI/API runs are now selected structurally by:
  - `sourceMode`
  - optional `compileProfile` for `mini`
  - `searchStrategy = { hoist, scheduler }`
- The app boundary adapts canonical flat configs into the internal
  `search-lattice` `+calls` machines before stepping.
- The default headless lane is modern-only.
- The eager/lazy/proceed ladder remains available only through the archived legacy lane.
- Supported lanes are `A`/`B`/`C`/`D`/`E` above.
