# Monoid

An associative binary operation with an identity

`mempty` produces the identity for monoidal type e.g. [], Nothing or (Sum 0).
`mappend` or `<>` appends two monoidal types together e.g:

    [1, 2, 3] <> [4, 5] == [1, 2, 3, 4,  5]
    (Sum 1) <> (Sum 4) == (Sum 5)

## Laws

### Identity

    [1, 2] <> mempty == id [1, 2]
    mempty <> [1, 2] == id [1, ]

### Associativity

    (a <> (b <> c)) == ((a <> b) <> c)

# Semigroup

Same as Monoid but without an identity

# Functor

Apply (lift) a function over a structure to transform the contents, leaving the
structure alone.

For `Either` it only applies to the `Right` data constructor. Similarly for
tuple pairs it only applies to the second member:

`fmap` or `<$>`

    fmap (+2) [1, 2] == [3, 4]
    (+2) <$> Just 2 == Just 4
    fmap (+2) Left "some string" == Left "some string"
    fmap (+2) Right 2 == Right

## Laws

### Identity

    fmap id == id

    id <$> [1, 2, 3] == [1, 2, 3]
    fmap id "hello" == "hello"

### Composition

    fmap (f . g) == fmap f . fmap g

    fmap (chr . ord) "hello"      == "hello"
    fmap chr . fmap ord $ "hello" == "hello"

### Structure Preservation

`fmap` does not change the structure of the argument

    length (fmap someFunc [1, 2, 3]) == length [1, 2, 3]
