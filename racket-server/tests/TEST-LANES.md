# Test Lanes

This directory has multiple intentional lanes. Keep them separate so failures are easier to triage.

## Lane A: Headless (default)

Runs deterministic/unit/property checks that do not require GUI interaction.

```sh
raco test racket-server/tests/test-all-headless.rkt
```

Includes:
- Core property/judgment checks
- Variant lattice + randomized variant checks
- Frontend example compatibility gate (surface programs must parse/lift into `L4` syntax)

## Lane B: App/API Regression

Runs the app-level test suite used for server behavior regression checks.

```sh
raco test racket-server/tests/test-all.rkt
```

## Lane C: Legacy Semantics (manual host lane)

Runs legacy Redex semantics and visual smoke tests.

```sh
raco test \
  racket-server/tests/test-reduction-relations.rkt \
  racket-server/tests/unit-tests.rkt \
  racket-server/tests/translator-tests.rkt \
  racket-server/tests/visual-tests.rkt \
  racket-server/tests/test-dmitry-and-dmitry.rkt
```

Optional interactive stepper:

```sh
racket racket-server/tests/visual-tests.rkt
```

## Notes

- `translator-tests.rkt` now executes a real suite (`TRANSLATOR-LEGACY`) instead of reporting "No tests run."
- Keep GUI/manual lane failures separate from headless lane failures when triaging.
