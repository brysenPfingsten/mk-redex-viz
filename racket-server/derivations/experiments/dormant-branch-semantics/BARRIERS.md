# Earlier dormant-branch semantics: limits of strict correspondence

Here "lattice" means this experiment's earlier dormant-branch
[source semantics](source/SEMILATTICE.md).
The current GUI uses strict matrix scheduler rows and no longer exhibits
these dormant-branch counterexamples. These diagnostics initialize and step
the historical sources explicitly.

The existing [S reference interpreter](../../s-reference/interpreter.rkt)
is not the earlier dormant-branch Railroad interpreter merely with different constructors. It evaluates
both disjunction operands, and the eager tail of a bind, before the resulting
Search can commit. The lattice can commit a settled candidate while a sibling
remains unevaluated. Changing only Strict's delayed-merge scheduling equation
does not remove this distinction.

Run the independent diagnostics from the repository root:

```sh
raco test racket-server/derivations/experiments/dormant-branch-semantics/tests/barriers-tests.rkt
```

The tests execute the existing native runtime relations. Their configurations
are checked against the native grammar and WF predicate. Strict's public
`advance` invocation is recorded separately from a named contraction. Finite
Strict results are also checked against the S reference direct interpreter.
These tests do not modify either semantics or evaluate a proposed
representation map to make the traces agree.

The historical sources retain `Last(Owners, Answer(Owners, state))`; the
current strict source has `Solo(Owners, state)`. Cross-account tests read each
native terminal directly into neutral `terminal-answer` observation data.
They do not convert a historical configuration into a current configuration
or add historical constructors to the current representation maps. The
separate source tests reuse current scope checking through an explicitly
static scope abstraction; that abstraction is never stepped.

## Positive evidence for the earlier dormant-branch semantics

The [dormant-branch interpreter](derivation/interpreter.rkt) changes demand as well as
scheduling. It is compared through its independently stepped
[source presentation](derivation/source.rkt), separately from the unchanged Strict
interpreter discussed below.

For all **20 existing validation witnesses under each of DFS, Flip, and
Railroad**, the tests require both the derived source and the matching native
lattice run to finish within an explicit bound. These 60 comparisons check:

- the dormant-branch direct interpreter's complete result equals its source result;
- derived source and native lattice have equal `world-frontier` observations,
  retaining Forced/Emit structure, terminal success/failure, answer order, complete ordered
  Owner groups and tags on every world path, and scoped logical states;
- their **ordered atomic work** agrees after the same scoped renaming of each
  attempt's Owner path, pending goal, substitution, disequalities, and trail.

Atomic work is observed at actual derived-source `eval-atom` contractions and all
native atomic rule families, including failure, unsuccessful unification,
and disequality outcomes. It is not reconstructed from successful answer
trails or compared as an unordered collection. Derived-source focus contexts come
from `decompose` followed by `plug(hole, frames)`; the ownership observer
handles both left-active `mplus` and right-active `mplusR`. Every source
configuration passes its grammar check and the decomposition/plugging round
trip; the native trace independently passes its grammar and WF checks.

These are positive finite observations with full ownership data. The twenty
fixtures use an empty relation environment; they do not prove arbitrary
recursive behavior or a full-configuration bisimulation. In particular,
`world-frontier` is the explicitly weaker observation described below, and
the negative test for raw ownership placement remains in place. The Strict
work-order and divergence counterexamples concern a different interpreter;
they are not failures of these dormant-branch comparisons.

## Work, commitment, and divergence

Here `A` and `B` are successful labelled equations. `commit A` means a new
answer on the public Frontier spine, not an intermediate Search candidate.

| Goal | Strict events | Lattice events |
|---|---|---|
| `A ∨ B` | work A, work B, commit A, commit B | work A, commit A, work B, commit B |
| `(A ∨ B) ∧ fail` | work A, work B, fail, fail | work A, fail, work B, fail |
| `suspend(A) ∨ B` | work B, advance, internal force, work A, commit B, commit A | Flip/Rail: public force, work B, commit B, work A, commit A |

No interpreter commits the candidates rejected by the pending bind. DFS's
left-delay case instead commits A before B. Exact native labels are checked;
for example, the lattice's `commit-choice-answer` precedes the second
`unify-success`, while Strict's `commit-yield` follows both `eval-atom` steps.

The first finite distinction is also tested through the compiler:

```racket
(run* (q) (conde [(== q 'a)] [(== q 'b)]))
```

Its fixed profile is conjunction left, disjunction right, delay placement
`relbody`. Compilation produces identical definitions, goal, initial Owners,
state, and query metadata for all four runtime choices. Only the runtime
configuration wrapper differs.

The distinction becomes an observable-answer counterexample when an eager
sibling never returns:

```racket
(defrel (spin) (spin))
(run* (q) (conde [(== q 'a)] [(spin)]))
```

Use the same association choices and delay placement `disj`. This profile
delays the disjunction; it does not delay the recursive call in `spin`'s
non-disjunctive body. After the outer public delay, Strict evaluates `q=a` but
commits no answer. Every lattice scheduler commits `a`, then enters `spin`.
The test checks the same compiled input in all four carriers and closes an
**exact deterministic source self-loop**: the sole `eval-call` or
`expand-relcall` successor equals its predecessor. The persistent answer
difference therefore does not depend on interpreting a fuel limit as proof
of divergence.

Consequently unrestricted Strict/Railroad equivalence preserving exposed
answers is false, even for a supported compilation profile. This is a genuine
strictness difference, not an apparent compiler bug. Any weaker positive
relationship must specify its productivity restriction and observations;
finite completed-result agreement alone does not prove it.

## A guarded scheduler discriminator

For `loop() = suspend(loop())`, compare `loop() ∨ B`. DFS repeatedly chooses
the left branch. Starting at its first paused Frontier, the tests check the
exact cycle

```text
force-delay → expand-relcall → suspend-goal → dfs-delay-left
```

The result is the same paused Frontier beneath one additional empty `Forced`
wrapper; B remains untouched. Closure under the public spine gives the
induction step for this particular starvation argument. The test also checks
60 native transitions and the absence of B work or committed answers.
Strict, Flip, and Railroad expose B within the same finite observation bound.
This is a program-specific recurrence and bounded positive evidence, not a
general fairness theorem.

## Freshness and what the observers erase

The lattice's `allocate-fresh` avoids every name in the **currently retained
Frontier**. Strict avoids names on the active ownership path. For two private
fresh branches, Strict can use `u:0` in both worlds; the lattice's retained
first answer makes the second branch choose `u:1`. After a failed branch is
discarded, the lattice can reuse its name. This is not a monotone global
allocation counter.

The allocation checks cover shared ancestors, private siblings, empty and
unused introductions, answer-local bind continuations, suspension, failure,
sparse inherited order, disequalities, and replay trails. Each active path's
ordered support determines one renaming applied consistently to its Owner
groups, pending goal, substitution, disequalities, and trail. Shared ancestors
keep their positions; private worlds extend independently. An unallocated
variable makes the observer fail. Allocation observations are compared as
multisets, explicitly forgetting their different global execution order.

The final `canonical-frontier` observer retains Owner groups and tags, branch
scope, and Frontier structure. Its terminal convention joins historical
Last's root Owners with its Answer Owners, because Last has no residual
world; it reads current Solo's Owners directly. Both produce the neutral
`terminal-answer` observation without changing either source representation.

Separate allocation observations forget Owner groups and tags, either
retaining each world's ordered named support or addressing its state
numerically. On current strict results, these are independently checked
against observations of the actual `Q-SE` and `Q-SN` images. Historical
results are observed directly and never passed to those current maps.
Numeric equality is insufficient on its own: two
reachable executions differing by an empty Owner group, or by one two-name
group versus two one-name groups, have equal numeric images but different
provenance observations. Tests assert those inequalities.

There is also a discrepancy beyond scoped alpha. In the existing
`delayed-sibling-capture` witness:

```text
Strict:    Forced(shared, Emit(empty, answer, rest))
Flip/Rail: Forced(empty,  Emit(shared, answer, rest))
```

The tests assert that these remain **different** under
`canonical-frontier`. A separate, explicitly weaker `world-frontier`
observer retains Forced/Emit shape and every leaf's complete ordered Owner
groups and state, but forgets which ancestor physically carries a shared
group. Those observations agree. Thus the evidence preserves the world's
introductions and provenance while relocating a group relative to the force
event; it is not merely a change of variable spelling. A correspondence that
identifies these values must state this scope-transport quotient. A richer
observation associating ownership with individual Forced nodes distinguishes
them.

These finite checks support scoped allocation compatibility for the tested
worlds. A general lemma still needs preservation of the branch scope relation
by allocation, kernel operations, bind, suspended work, and relation
instantiation, including unused groups and exact trails. The negative
commitment and divergence results remain regardless of that lemma.
