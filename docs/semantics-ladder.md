# Semantics Organization

This repo now has two semantic views living side by side:

- the legacy/public `L0 -> L4` ladder under `racket-server/src/languages`,
  `racket-server/src/wf`, and `racket-server/src/reduction-relations`
- the internal feature-based search lattice under
  `racket-server/src/search-lattice`

The app/API boundary now runs through the internal search lattice.

## Public Ladder

The canonical parser target is still the legacy flat `L4/config` shape.

```mermaid
flowchart TD
  L0["L0"]
  L1["L1 calls + delay"]
  L2["L2 left disjunction"]
  L3["L3 base"]
  L4["L4 railroad"]

  L0 --> L1
  L0 --> L2
  L1 --> L3
  L2 --> L3
  L3 --> L4
```

This ladder remains useful for:

- canonical parsing and transpilation
- legacy/public reduction relations
- legacy/public well-formedness checks

## Internal Search Lattice

The internal stepper family is organized by features instead of numbered levels:

- `core`
- `delay`
- `disj`
- `search-base`
- `search-dfs`
- `search-flip`
- `rail`
- `calls` as an overlay

The important split is that the search lattice is call-free until the `calls`
overlay is added.

```mermaid
flowchart TD
  CORE["core"]
  DELAY["delay"]
  DISJ["disj"]
  DISJSEQ["disj/seq"]
  DISJFUSED["disj/fused"]
  SBSEQ["search-base/seq"]
  SBFUSED["search-base/fused"]
  DFSSEQ["search-dfs/seq"]
  DFSFUSED["search-dfs/fused"]
  FLIPSEQ["search-flip/seq"]
  FLIPFUSED["search-flip/fused"]
  RAILSEQ["rail/seq"]
  RAILFUSED["rail/fused"]
  CALLS["calls overlay"]

  CORE --> DELAY
  CORE --> DISJ
  DISJ --> DISJSEQ
  DISJ --> DISJFUSED
  DELAY --> SBSEQ
  DELAY --> SBFUSED
  DISJSEQ --> SBSEQ
  DISJFUSED --> SBFUSED
  SBSEQ --> DFSSEQ
  SBFUSED --> DFSFUSED
  SBSEQ --> FLIPSEQ
  SBFUSED --> FLIPFUSED
  SBSEQ --> RAILSEQ
  SBFUSED --> RAILFUSED
  DELAY --> CALLS
```

## Hoist Axis

The key semantic axis added by the internal lattice is hoisting:

- `seq` means early hoist / staged contexts
  - pending conjunction is distributed across visible disjunction promptly
  - runtime contexts use `K` plus `KDisj`
- `fused` means late hoist / mixed contexts
  - mixed `conj/disj` shapes can remain stable longer
  - runtime contexts extend `K` directly

This hoist question is separate from scheduler policy:

- `dfs`
- `flip`
- `rail`

and separate from source compilation choices such as delay placement.

## Why The App Uses The Search Lattice

The GUI/API boundary now uses structured strategy selection:

- `sourceMode`
- optional `compileProfile` for `mini`
- `searchStrategy = { hoist, scheduler }`

The app flow is:

1. parse/transpile surface input to canonical flat `L4/config`
2. validate that canonical target
3. adapt the flat config to the internal `+calls` search-lattice shape
4. run relation-aware internal `wf`
5. step using the reducer selected by `searchStrategy`

The app/runtime seam lives in:

- `racket-server/src/search-strategy.rkt`
- `racket-server/src/search-runtime.rkt`

## Stable Conclusions Worth Carrying

- The early-vs-late hoist question already appears at the plain
  conjunction/disjunction layer. It is not fundamentally about relation calls.
- Delay/suspension and scheduler policy are separate axes from hoisting.
- The internal search lattice makes those axes explicit without surfacing every
  intermediate machine in the GUI.
- Relation calls are better understood as an overlay on delayed search, not as
  part of the core search lattice itself.
