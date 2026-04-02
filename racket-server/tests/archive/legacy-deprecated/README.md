# Legacy Test Archive

This archive was aggressively pruned after retiring the old legacy
`parse-prog` pipeline. The archived parity harness, legacy stack, archived
`dmitry-and-dmitry` relation, and leftover `synthesizer.rkt` sketch were all
removed.

The directory now remains only as a historical marker; no archived legacy
pipeline files remain here.

Active replacement suites:
- Core + variant semantics checks: `racket-server/tests/test-all-headless.rkt`
- Variant feature/rule checks: `racket-server/tests/variant-module-tests.rkt`
- Transpiler canonical target checks: `racket-server/tests/test-transpiler.rkt`
- App/API + integration flow: `racket-server/tests/test-all.rkt`
