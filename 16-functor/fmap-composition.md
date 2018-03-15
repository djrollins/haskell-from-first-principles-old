Given:

    (.)  :: (b -> c) -> (a -> b) -> (a -> c)

    fmap :: Functor f => (m -> n) -> f m -> f n
    fmap :: Functor g => (x -> y) -> g x -> g y

Substitue first `fmap`:

    `(b -> c)` with `Functor f => (m -> n) -> f m -> f n`
    `b`        with `(m -> n)`
    `c`        with `(f m -> f n)`

    (Functor f) =>
        ((m -> n) -> (f m -> f n) ->
        (a -> (m -> n)) ->
        (a -> (f m -> f n))

    (Functor f) =>
        (a -> (m -> n)) ->
        (a -> (f m -> f n))

Substitute second `fmap`:

    `a -> (m -> n)` with `Functor g => (x -> y) -> g x -> g y`
    `a`             with `(x -> y)`
    `m`             with `g x`
    `n`             with `g y`

    (Functor f, Fuctor g) =>
        ((x -> y) -> (g x -> g y) ->
        ((x -> y) -> f (g x) -> f (g y))

    (Functor f, Fuctor g) =>
        ((x -> y) -> f (g x) -> f (g y))

Gives:

    (fmap.fmap) => (Functor f, Fuctor g) => ((x -> y) -> f (g x) -> f (g y))
