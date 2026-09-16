# Corrections and retained lessons

This log records the questions that changed the account and the small
maintained witnesses that make the answers inspectable. The
[research guide](README.md) owns current implementation and proof status.

| Mistaken assumption or question | Correction | Current witness |
| --- | --- | --- |
| Are S introductions just decorations on returned answers? | Introductions supply allocation support while computation runs. Unused and empty introduction groups matter; common ancestry and an answer's private ancestry have different descendants. Internal force retains the removed Delay's groups on active computation before fresh allocation. E/N keep the same allocation world in their states and need no identity prefix phase. | `unused-binder`, `shared-outer`, `sibling-reuse`, `delayed-sibling-capture`, and `sparse-inherited-ancestry` in [witnesses.rkt](test-support/witnesses.rkt); owner lifting in [source-tests.rkt](s-reference/source-tests.rkt) and [checkpoint S/E/N transitions](matrix/s-reference-tests.rkt) |
| Can completion always be encoded as an emitted answer followed by empty work? | `Done` retains terminal failure/allocation structure; `Solo` owns a terminal answer state directly. `Solo(O,σ)` and `Emit(O,Answer(∅,σ),Done(∅))` are structurally distinct even when their answer lists agree. Exact Frontier structure includes failed worlds and empty introduction groups. | Terminal-form assertions in [constructor-tests.rkt](constructor-tests.rkt); `allocated-failure`, `failed-sibling`, and `nested-rail` in [witnesses.rkt](test-support/witnesses.rkt); exact boundaries in [interpreter-tests.rkt](s-reference/interpreter-tests.rkt) |
| Does a successful active Search candidate already justify settled output? | Pending bind can fail or suspend. `Yield` is active Search; only commitment builds Frontier answers. Unary `More(Delay(...))` retains unfinished Frontier work. | `intermediate-success-then-failure` and `delayed-continuation-schedule` in [witnesses.rkt](test-support/witnesses.rkt); phase rejection in [machine-correspondence-tests.rkt](s-reference/machine-correspondence-tests.rkt) |
| Can matching completed answers justify dormant-right scheduling? | Strict disjunction matures both operands left to right; eager bind processes the continuation and residual before commitment. Delaying right-hand work changes the operation order even when completed output agrees. | `strict-sibling-maturation` and `eager-bind-residual` in [witnesses.rkt](test-support/witnesses.rkt); [policy-tests.rkt](experiments/dormant-branch-semantics/tests/strict-policy-tests.rkt) compares strict work order with the earlier dormant-branch semantics |
| Are all suspensions interchangeable thunks, including the host trampoline? | `REval`, `RMerge`, and `RBind` retain specific pending computation. Saved right Search and nested choice orientation matter. Only object-language Delay suspends that work; host tail-call dispatch is administrative. | `bind-delayed-left`, `delayed-sibling-capture`, and `nested-rail` in [witnesses.rkt](test-support/witnesses.rkt); [data.rkt](s-reference/data.rkt), [administrative rank](s-reference/administration.rkt), and [register span checks](s-reference/register-compression-tests.rkt) |
| Must the interpreter be the fixed starting point for the derivation? | The syntactic machine helped reconstruct the S reference interpreter and its resumption interface. Forward CPS, defunctionalization, and machine generation then checked the connection through explicit configurations and prescribed steps. | [source-to-interpreter reading path](s-reference/README.md#inspectable-path-from-syntax-to-interpreter), [show-machines.rkt](s-reference/show-machines.rkt), and [configuration checks](s-reference/machine-correspondence-tests.rkt) |
| Must relation calls introduce Delay or rely on hidden host recursion? | First-order named calls expand eagerly. Suspension belongs to explicit compiled syntax. Γ is retained in program terms, pending goal captures, data frames and resumptions; recursive calls do not change strict operand order. | [Full source/stage checks](matrix/full-tests.rkt), [functional relation checks](s-reference/relation-tests.rkt), and [finite Big relation checks](matrix/big/full-tests.rkt) |
| Should run n stop as soon as an answer head becomes visible? | Positive limits wait for the next exposed Delay or terminal Frontier. Every current GUI scheduler finishes its strict round and commitment. An unguarded residual can prevent reaching that boundary. The result is a prefix of the saved Frontier; manual stepping ignores the limit. | [Automatic driver checks](../tests/program-runner-tests.rkt) and [manual API boundaries](../tests/test-app.rkt) |
| Can normalized-tree positions identify original source occurrences? | Lowering a multi-clause `conde` adds operators and reassociation moves them. Source IDs must precede lowering; generated operators inherit their enclosing source form, while repeated source leaves remain distinct. | Source-attribution cases across all twelve compiler profiles in [test-transpiler.rkt](../tests/test-transpiler.rkt) |
| Does the strict correspondence supersede the GUI's three runtime choices? | No Interleave, Flip-Flop, and Railroad remain essential schedulers, separate from compiler association and Delay placement. The current extension uses strict `mplus`; Railroad adds `mplusR` and eager `YieldR`. It does not reinstate dormant `DisjL`/`DisjR` execution. The strict reference view shares Flip's source; downstream DFS/Railroad derivations remain open. | [Strict scheduler equations and map](matrix/scheduler-source.rkt), [scheduler checks](matrix/scheduler-tests.rkt), and [runtime/application checks](../tests/search-runtime-tests.rkt) |
| Does a source being selectable by an application make it application plumbing? | Source semantics and machines belong in `derivations/`; `src/` compiles, selects, runs and presents them. A superseded semantic account keeps its source, derivation and account-specific tests together in `experiments/`. Neutral shared providers stay outside those accounts. | [Current inventory](README.md), [experiments](experiments/README.md), and [layout contracts](layout-tests.rkt) |

## Construction lessons

Terminal success now uses `Solo(owners, state)`. The former strict reductions
always produced `Last(Owners(), Answer(owners, state))`; permitting a nonempty
outer `Last` owner field overstated the reachable representation. Tightening
the grammar removes that empty container without merging candidate `One`
with committed `Solo`, or erasing the common/private scope distinction in
`Yield` and `Emit`. The earlier dormant-branch experiment keeps its own `Last`
syntax. [Constructor checks](constructor-tests.rkt) reject the old shape in
the current account; the matrix and functional-machine checks retain exact
scope and commitment boundaries.

These passages retain wording from the early refactoring notes, with examples
updated to the current syntax.

- **Keep semantic roles grammatical.** Some constructors belong to more than
  one nonterminal. That overlap is acceptable as long as reductions and renderers
  know which role they mean from context or nonterminal position. For conjunction,
  the left child must stay active computation, not arbitrary observation.
  The [strict grammar](shared/grammar-s.rkt) expresses this as `bind` over `c`;
  committed Frontiers belong to `o`.
- **Inherit recursive context extensions.** `define-union-language` does merge
  recursive context extensions extensionally. Because of that, we should not
  restate a combined recursive context at a higher layer unless we are actually
  changing its shape. If the intent is just to inherit two independent extensions,
  the union language already gives that. The
  [earlier Search language](experiments/dormant-branch-semantics/source/languages/search-lang.rkt)
  uses that construction. This grammar fact does not supply an interaction rule:
  [strict Search](matrix/README.md#feature-ownership-and-scheduler-policy)
  additionally requires `mplus-delay`.
- **Distinguish syntax from policy.** If two language layers are syntactically
  identical and differ only in their reducers, that distinction belongs in the
  reduction-relations layer rather than in separate language modules.
  [No Interleave and Flip-Flop](matrix/scheduler-source.rkt) share `StrictSRel`;
  Railroad extends the syntax to retain orientation.
- **Keep inherited rule identities stable.** Reduction-rule names like
  `core/...`, `delay/...`, and similar language-provenance prefixes are useful
  temporary scaffolding while the lattice is still being corrected and debugged,
  because they make blame and search easier. They are not the desired final
  naming style. Once the semantics stabilize, those provenance annotations
  should be removed so the same inherited rule keeps the same language-neutral
  identity all the way up the lattice. The current
  [feature checks](matrix/feature-tests.rkt) compare inherited rule labels;
  [Railroad's explicit label map](matrix/scheduler-source.rkt) accounts for its
  added orientation cases.

The open [WF design question](matrix/README.md#how-strong-should-wf-be)
is retained with the current domain contract. The obsolete constructor/context
plan and its collector-removal instructions are retired; today's `commit`
and `collect` remain semantic operations.

## Retired and deferred results

The [early conjunction distribution experiment](experiments/early-conjunction-distribution/README.md)
remains executable as a separate policy investigation. Distributing pending
conjunction through nested choice changes both answer-state order and the
first-answer Delay boundary in its [finite witness](experiments/early-conjunction-distribution/tests.rkt).
It is therefore not a representation stage of this strict derivation, and its
relocation does not integrate it into the matrix or functional pipeline.

Checkpoint commit `0a3a075` preserves the pre-cleanup implementation, tests,
generators, and documentation. Historical locations below are plain Git paths
at that commit, not links to maintained files. Removing their comparison
suites deliberately reduces coverage; the retained gates do not inherit
every theorem statement or witness from those routes.

- **Older numeric functional, denotational, and a7–a9 routes:** their distinct
  presentations are retired after keeping the current control, allocation,
  phase, work-order, and generator assertions in the selected account.
  History: `racket-server/derivations/functional-search/`, the numeric modules
  directly under `racket-server/derivations/strict-search/`, and its
  `denotational/` and `a7-a9/` directories at `0a3a075`.
- **Explicit-prefix S functional route and ownership-erasure bridge:** the
  parallel interpreter-to-register pipeline and tests that existed only to
  compare it with retained scope are retired. The native matrix now carries
  retained scope through S/E/N source, stage, and finite Big equations;
  its earlier syntactic prefix phase and frames have been removed. History:
  `racket-server/derivations/strict-search/s-functional/` and
  `racket-server/derivations/strict-search/retained-scope/source.rkt`
  (`erase-prefixes`) at `0a3a075`.
- **Host-procedure fresh and recursion diagnostics:** the older host Fresh Ω
  examples and their proof-search-exhaustion diagnostics remain deferred;
  their host-procedure domain is not the selected lexical goal syntax.
  First-order relation calls, recursion and mutual recursion are now
  implemented separately in the [full source](matrix/full-source.rkt) and
  [functional derivation](s-reference/relations.rkt). Their checks include
  bounded productive prefixes and unguarded divergence, not a general stream
  theorem or recovery of those older host-procedure results. History:
  `racket-server/derivations/functional-search/direct-interpreter.rkt`,
  `racket-server/derivations/strict-search/tests.rkt`, and
  `racket-server/derivations/strict-search/big-step-spec.rkt` at `0a3a075`.
- **Older numeric Big certificates:** source-specific proof-search results,
  R/B/M certificates, and their exhausted-search distinction are deferred.
  History: `racket-server/derivations/strict-search/big-step.rkt`,
  `big-step-spec.rkt`, and their tests at `0a3a075`.
  The maintained [native matrix Big](matrix/big/README.md) retains its own
  independent judgments, fixed-point equations, and recursive S/E/N
  certificates for the aligned retained-scope sources. These finite judgments
  do not establish productivity or recover the older numeric proof-search
  exhaustion results.
- **Old refocusing playground:** memoized-`c` reconstruction and erase/restore
  results over the earlier FreshenedTree/rail-fused carrier remain historical.
  History: `racket-server/derivations/refocusing/` at `0a3a075`. The maintained
  [stage machinery](shared/stages/schema.rkt) is the current derivation surface;
  it does not claim those carrier-specific results.

The retired functional routes above remain in Git history. The default GUI
runs strict oriented Railroad, retaining No Interleave and Flip-Flop.
The strict reference (Flip) is a separate view and the default API/library
selection. All current histories retain `(program Γ q)` and explicit public
`advance`; internal `force-delay` remains a reduction. The
[current picture projection](../src/search-picture.rkt) reads strict syntax,
preserving candidates, Done/Solo, and Owner groups. Dormant work-tree
inspection belongs to the [experiment](experiments/dormant-branch-semantics/README.md).
Automatic consumption lives in
[minikanren.rkt](../src/minikanren.rkt), separate from manual session control.

The historical strict “Search/rail” name does not identify its equations with
the oriented Railroad carrier. Partial interpreter correspondence is an
accepted application boundary. The dormant source remains a real
dependency of the independent early-distribution experiment. Keeping these sources
executable supplies no online-fusion theorem; that requires its own
observation and hypotheses.
