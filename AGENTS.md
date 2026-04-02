# Agent Notes

## Racket `in-dict` + `for` binding reminder

When iterating dictionaries in Racket, prefer direct key/value binding in the `for` clause:

```racket
(for/list ([(k v) (in-dict h)])
  (format "~a = ~s" k v))
```

Use this style instead of adding extra local destructuring in the loop body.

## Racket recursion style reminder

When writing recursive helpers over term/list trees, prefer direct recursive functions with an optional accumulator parameter over introducing an internal `let loop`.

Preferred shape:

```racket
(define (f x [acc init])
  (match x
    ['() acc]
    [(cons a d) (f a (f d acc))]
    [_ acc]))
```

Use this especially when replacing non-tail-recursive `append` patterns.

## Data-shape discipline (avoid gratuitous conversions)

Before introducing a new container (`set`, `hash`, etc.), first use the structure already in hand unless there is a clear asymptotic or clarity win.

Guidelines:

- If you already have a list and only need occasional membership tests, prefer `member` over building a one-off set.
- Convert list -> set/hash only when you reuse that conversion enough times to justify it.
- In review, remove helper state that does not simplify logic or improve complexity in a measurable way.
- Prefer the smallest coherent change over speculative optimization.

## Naming discipline (no alias stubs)

Avoid rinky-dink alias definitions whose only job is renaming, e.g.:

```racket
(define new-name old-name)
```

Guidelines:

- Prefer renaming call sites/imports/exports directly over introducing bridge aliases.
- If a compatibility name is truly required, keep it at module boundaries via `provide`/`require` renaming rather than internal runtime aliases.
- When an alias appears during refactor, treat it as temporary and remove it before check-in unless there is a documented compatibility need.
