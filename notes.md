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

It is a mapping between categories.

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

`fmap` does not change the structure of the argument e.g the lenngth of the
output list is equal to the length of the input list. Similary, if the argument
is `Just`, the result is `Just`. A `Just` argument cannot produce `Nothing`.

    length (fmap someFunc [1, 2, 3]) == length [1, 2, 3]

# Applicative

Application of a function inside a structure to an argument inside a structor
of the same type.

Known as a *Monoidal Functor*.

The Functor part comes from the fact that we're applying a function to
something inside a structure without affecting the structure (`fmap`)

The Monoidal part comes from the fact that we have two things in two structures
and are bringing them together (`mappend`)

`<*>` or *apply*, applies functions in the structure on the right to arguments
in structure on the left.

    [(1+), (2*)] <*> [(1+1), (1+2), (1+3), (2*1), (2*2), (2*3)] = [2, 3, 4, 2, 4, 6]
    Just (+1) <*> Just 2 = Just 3
    Nothing   <*> Just 2 = Nothing

`pure` wraps something in the Applicative instance:

    pure 1 :: Maybe Int = Just 1
    pure (+1) :: [a -> b] = Just (+1)

`pure` and `<*>` are equivalent to `fmap`

    pure (+1) <*> [1, 2] == [2, 3]
    fmap (+1)     [1, 2] == [2, 3]

# The `fmap`-apply idiom

Consider a smart constructor that Maybe constructs a type from lots of Maybe arguments:

    validateName :: String -> Maybe String
    validatePositive :: Integer -> Maybe Integer

    data Cow = Cow String Integer Integer

    createCow :: String -> Integer -> Integer -> Maybe Cow
    createCow name' age' weight' =
        case validateName name' of
            Nothing -> Nothing
            Just nammy ->
                case validatePositive age' of
                Nothing -> Nothing
                Just agey ->
                    case validatePositive weight' of
                    Nothing -> Nothing
                    Just weighty ->
                        Just (Cow nammy agey weighty)

Instead, you can lift the Cow data constructor into a maybe with `pure` and
then apply it to the arguments with `<*>`:

    createCow name' age' weight =
        pure Cow <*> validateName name'
                 <*> validatePositive age'
                 <*> validatePositive weight'

Or you can use `fmap` to create partially-applied data constructor in a Maybe:

    createCow name' age' weight =
        Cow <$> validateName name'
            <*> validatePositive age'
            <*> validatePositive weight'

Or use the `liftA` functions:

    createCow name' age' weight =
        liftA3 Cow (validateName name')
                   (validatePositive age')
                   (validatePositive weight')

## Laws

### Identity

Same as Functor identity law:

    pure id <*> v = v

### Composition

Also same as Functor but has wierder syntax because `(.)` needs to be lifted
and `<*>` is an infix operator:

    let p1 = Just (+1)
    let x2 = Just (*2)
    let j3 = Just 3

    pure (.) <*> p1 <*> x2 <*> j3 = Just 7
    x <*> (y <*> z) = Just 7
    pure (.) <*> p1 <*> x2 <*> j3 = x <*> (y <*> z)

### Homomorphism

Is a structure-preserving map between two algebraic structures. The result has
to be contained within the same structure as the fuction and the argument(s).

Is different to the *Structure Preserving* law for Functor in that the
resulting structures are monoidal and have to come together. `fmap`ing over a
list means the resulting list is the same length as the input list, whereas
`<*>`ing a list of functions over a list of arguments results in a length that
is the product of the length of the two input lists.  Likewise, with `fmap` the
`Just`-ness or `Nothing`-ness of the argument is maintained; whereas, with
`<*>`, `Just` can become `Nothing` if the function is `Nothing`.

    pure f <*> pure x == pure (f x)

### Interchange

    u :: Maybe (Int -> Char)
    x :: Int

    u <*> pure x == pure ($ x) <*> u

The function application operator `$`, can be partially applied like any other:

    ($ 3) == \f -> f 3
    (f $) == \x -> f x

    u <*> pure x == pure (\f -> f x) <*> u

Lifting a value into the structure is the same as lifting function application
into the structure? I am unsure of the significance of this law...
