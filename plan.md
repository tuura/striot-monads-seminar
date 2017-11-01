1. Why control even side effects?

    * Modular semantics for effectful computations.
    * Referential transparency for "imperative" programs.

2. How to control side effects?

    * **Functor** to `map` over a container.
    * **Applicative** for `multi argument function` computed in an effectful environment.
    * **Monad** to `sequence` several effectful functions, feeding the out of
    first to the second. A `programmable semicolon`.

3. What benefits we may have in Haskell?

    * Concise and expressive code, but still type-safe code.
        + Reader example: passing around a variable as a parameter vs. Reader Monad.
        + Generation random numbers in a purely functional fashion.
        + List monad example + list comprehensions.
        + Alga example.
    * Pretty "imperative"-like DSLs via `do`-notation.
        + Redfin example.
    * Better, effect-aware, truthful programming practice.
        + A glance on monad transformers, mtl, and algebraic effects.