# L3 Derivation Side Project

This directory is an off-to-the-side derivation playground for a single
miniKanren fragment:

- pure L3
- no relation calls
- deterministic rail-style fair interleaving

It is intentionally separate from the production search-lattice runtime. The
goal is to make the existing `c`-carrying machine look like the endpoint of a
derivation instead of the starting point.

## Artifact Map

There are four executable artifacts.

1. `premachine/`
   This is the context-first reduction semantics. Pending control stays in
   reduction contexts as much as possible. The local contract step is allowed
   to inspect the matched context in order to recover ambient scope.

2. `cfree/`
   This keeps the current-style machine vocabulary but removes the memoized
   local `c` fields. The machine state is still explicit about
   `FreshenedTree`, `FreshenedShell`, `Bounced`, and committed shell shape, but
   ambient scope is recovered from context and only restored at the bridge
   boundary when we compare against the current localized machine.

3. `zipper/`
   This is the focused/refocused view of the premachine semantics. It makes the
   decomposition result into an explicit machine state `(machine focus ctx
   obs)`. The `obs` field is a small observable snapshot slot, while `ctx`
   carries the control frames that were implicit in the premachine reduction
   contexts.

4. `bridge/`
   This contains the translations and executable correspondence checks. The
   bridge targets the existing no-calls fair interleaving endpoint, namely the
   current `rail-fused` reducer. The key bridge story is:

   - start with the context-first premachine semantics
   - refocus it into the zipper machine
   - localize control into the current-style c-free machine vocabulary
   - memoize ambient scope as local `c`

## Why Rail-Style Fair Interleaving

The side project fixes one deterministic interleaving policy so that the four
artifacts can be compared directly on traces and final observables. The chosen
policy is the current `rail-fused` endpoint because it is the rail-family
no-calls reducer that advances the shared delayed-interleaving corpus without
stranding the machine in an intermediate `Bounced` state.

## Where `c` Appears in the Current Machine

In the current machine, `c` appears in two operationally local places:

- inside `state`
- inside pending conjunction work nodes `(search × g c)`

Those fields duplicate information that is already recoverable from the
surrounding freshening context. In this side project, the bridge layer makes
that explicit:

- `erase-c` removes the memoized `c`
- `restore-c` reconstructs it from ambient scope

The point is not that the current machine is wrong. The point is that it looks
like a localized optimization of a context-sensitive precursor.

## Shared Kernel

Only a tiny logic kernel is shared:

- logic-variable generation relative to a used-scope list
- substitution walking and unification
- disequality invalidation
- source-level substitution for `fresh`

Everything else in this side project is intentionally duplication-friendly and
artifact-local.

## Tests

The tests live in `tests/pipeline-l3-tests.rkt` and are intentionally separate
from the main runtime test suites. They check:

- determinism on a shared L3 corpus
- agreement on final observable answers
- zipper/premachine step correspondence
- `erase-c` / `restore-c` round trips on current traces
- scope agreement of the current machine's memoized `c`

Run them with:

```bash
racket racket-server/derivations/pipeline/tests/run.rkt
```
