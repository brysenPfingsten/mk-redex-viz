# Legacy Test Archive

These test files are archived and no longer part of the supported test lanes.

Archived files:
- `test-dmitry-and-dmitry.rkt`
- `judgment-parity.rkt`
- `legacy-stack/test-reification.rkt`
- `legacy-stack/test-metafunctions.rkt`
- `legacy-stack/test-microkanren-reductions.rkt`

Notes:
- `judgment-parity.rkt` is retained as a historical diagnostic. It reports legacy/canonical disagreements but no longer acts as a supported parity gate.

Archived source dependencies:
- `racket-server/src/archive/legacy-deprecated/core-judgment-forms.rkt`
- `racket-server/src/archive/legacy-deprecated/legacy-stack/definitions.rkt`
- `racket-server/src/archive/legacy-deprecated/legacy-stack/judgment-forms.rkt`
- `racket-server/src/archive/legacy-deprecated/legacy-stack/metafunctions.rkt`
- `racket-server/src/archive/legacy-deprecated/legacy-stack/reification.rkt`
- `racket-server/src/archive/legacy-deprecated/legacy-stack/reduction-relations/dfs.rkt`
- `racket-server/src/archive/legacy-deprecated/legacy-stack/reduction-relations/reduction-relations.rkt`
- `racket-server/src/archive/legacy-deprecated/legacy-stack/utility-judgment-forms.rkt`
- `racket-server/src/archive/legacy-deprecated/synthesizer.rkt`
- `racket-server/src/reduction-relations/archive/legacy-deprecated/dmitry-and-dmitry.rkt`

Active replacement suites:
- Core + variant semantics checks: `racket-server/tests/test-all-headless.rkt`
- Variant feature/rule checks: `racket-server/tests/variant-module-tests.rkt`
- Transpiler canonical target checks: `racket-server/tests/test-transpiler.rkt`
- App/API + integration flow: `racket-server/tests/test-all.rkt`
