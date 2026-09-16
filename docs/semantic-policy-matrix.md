# Semantic policy and integration status

The [strict derivation inventory](../racket-server/derivations/README.md)
records the maintained derivations and remaining proofs. The
[organization guide](semantics-ladder.md) connects them to the application;
the [correction log](../racket-server/derivations/CORRECTIONS.md)
keeps the history of superseded choices.

The GUI must preserve **No Interleave, Flip-Flop, and Railroad** as runtime
choices. They are part of the intended application, independently of the
compiler's associativity and delay-placement controls. All three now operate
within strict maturation and commitment. The earlier dormant-branch semantics
remain comparison sources, not GUI implementations.

| Account | Current role | Operational contract |
| --- | --- | --- |
| [S reference](../racket-server/derivations/s-reference/README.md) | Selected interpreter, independently stated source, corresponding machines, registers and first compression | Strict disjunction, eager Yield tails and bind; explicit commitment; only Delay suspends |
| [Native S/E/N matrix](../racket-server/derivations/matrix/README.md) | Twelve call-free representation/feature cells and three full relation cells through source, data stages and finite Big | Same retained-scope operations, expressed using syntax-owned introductions, state support, or numeric supply |
| [Strict scheduler lattice](../racket-server/derivations/matrix/scheduler-source.rkt) | GUI default Railroad; No Interleave (`dfs`), Flip-Flop (`flip`), and Railroad (`rail`) remain runtime choices | Strict `(program Γ q)` configurations; eager merge/bind; Railroad adds native `mplusR` and eager `YieldR` |
| [Strict Search view](../racket-server/src/search-runtime.rkt) | Separate GUI view and default API/library selection; executes `strict-s-rel-red` directly | Native `(program Γ q)` syntax, explicit calls and exact source steps |
| [Earlier dormant-branch semantics](../racket-server/derivations/experiments/dormant-branch-semantics/README.md) | Complete alternative account: native sources, interpreter/machine derivation, and its tests | Deferred operands and Yield/bind tails; fragment correspondence and historical strictness counterexamples. This is not the current GUI's demand policy. |

Directory ownership follows these roles. `src/` contains application plumbing;
the current strict account is directly under `derivations/`. Each experiment
owns its source and evidence under `derivations/experiments/`. Shared semantic
primitives and neutral test support remain outside the experiments. The strict
aggregate excludes experimental comparisons; the headless aggregate invokes
both the strict and experiments gates.

The GUI sends an explicit lattice scheduler; Strict Search sends
`{ "model": "strict" }`. Omitting selection at the API/library boundary uses
the distinct `(strict-search)` model. Strict Search is not a fourth scheduler,
and its reference Flip relation is also the lattice's `flip` row. Railroad
retains orientation through a strict grammar extension. The historical
“Search/rail” feature name does not itself designate that extension.

## Choices that must remain separate

- **Compiler profiles:** two conjunction associations × two disjunction
  associations × three delay placements gives twelve compiled goal shapes.
  These choices may change work order or delay rounds; the finite comparison
  witness does not prove all profiles observationally equivalent.
- **Runtime scheduling:** No Interleave retains the active left branch at a
  delay; Flip-Flop exchanges merge arguments. Railroad
  represents orientation explicitly with `mplus` and `mplusR`. This grammar
  extension is part of its operational account, not an associativity or
  delay-placement compilation flag.
- **Representation and features:** S/E/N × Core/Delay/Disjunction/Search gives
  twelve call-free cells. Full Search with relations adds three more cells.
- **Derivation stages:** R/D/Z/M/B/Big and functional/register representations
  expose the same operations through different data and transition functions.
- **Consumption:** manual stepping can continue past a source `run n` request;
  the automatic consumer in [minikanren.rkt](../racket-server/src/minikanren.rkt)
  handles positive limits at the next exposed Delay or terminal Frontier.
  It does not stop just because an answer is visible; all schedulers finish
  their current strict round and commitment first.

Relation expansion adds no implicit Delay. Γ is explicit in the program and
retained data frames; suspension is present in the compiled goal itself.
Query-variable identities come from compiler metadata, not a scan of a changing
search tree. The [picture projection](../racket-server/src/search-picture.rkt)
distinguishes active candidates from committed answers and retains Done/Solo,
common/private introductions, and exposed delayed residuals.
All choices share compiled goals, source identity, and the strict program
carrier. Public advancement records `advance` before reduction; internal
`force-delay` never becomes a public scheduler tick. The renderer preserves
Railroad positions and highlights actual strict operand evaluation.

## Scope of correspondence

The new strict S scheduler source reuses the matrix Flip relation exactly.
Railroad has an orientation map with prescribed single-step checks, including
allocation scope and full relation calls. DFS has a distinct delayed-merge
equation. Separate E/N scheduler rows, downstream derivations and universal
proofs remain open; no strict-to-online fusion is used by the GUI.

The derivations of the [earlier dormant-branch semantics](../racket-server/derivations/experiments/dormant-branch-semantics/README.md)
distinguish demand policy from branch orientation. Their derived Railroad
source erases to its Flip source, and the native Railroad carrier independently
erases to native Flip through an exact named-step map in the checked corpus.
This preserves work and answer scheduling while forgetting stable branch
positions. This does not identify the strict interpreter with dormant-branch Railroad:
finite work/commit traces differ, and a compiled unguarded recursive suffix
disproves unrestricted exposed-answer equivalence. Full S ownership placement
also needs a scope-transport relation beyond variable alpha-renaming.

The two S reference derivations and native S/E/N machines have structural maps
and configuration-level transition checks, including the full relation
extension. Register decoders and compressed steps use prescribed original
spans. Big equations and certificate maps provide independent finite evidence.
This does not supply separate E/N functional or register derivations, universal
correspondence proofs, or productive-stream theorems.

The accepted GUI scheduler family need not have a completed interpreter
correspondence before it is usable. Each implemented correspondence must name
the scheduler, representation, and observation boundaries it covers. The
current strict checks do not establish equivalence of DFS, Flip, and oriented
Railroad. Native application integration does not complete that correspondence.

Strict-to-online fusion is a different prospective theorem. It can move finite
sibling work across commitment; without guardedness, `success(A) ∨ Ω` already
distinguishes answer prefixes. The current strict runtime does not rely on
that fusion, and no `κ / Q / π` compression is claimed. Tests relative to the
earlier dormant-branch semantics do not establish the current strict
application's contract.

The [distributed-conjunction experiment](../racket-server/derivations/experiments/early-conjunction-distribution/README.md)
remains a separate source-policy experiment. Distributing pending conjunction
into branches is a different question from retaining the three GUI schedulers;
the experiment is not integrated into the matrix or application.
