# Valid Functor Instances?

A type can only be an instance of Functor if it is of kind `* -> *`.

1) `data Bool = False | True`

Not possible possible becuase Bool is of kind *

2) `data BoolAndSomethingElse a = False' a | True' a`

Yes. BoolAndSomethingElse is of Kind `* -> *`.

3) `data BoolAndMaybeSomethingElse = Falsish | Truish a`

Yes. BoolAndMaybeSomethingElse is of Kind `* -> *`.

4) `newtype Mu f = InF { outF :: f (Mu f) }`

Mu is of Kind `(* -> *) -> *` as `f` is of kind `* -> *`. Any type parameter
given to Mu seems to produce a type of kind `*`. No Mu type can be of kind
`* -> *` therefore it cannot be an instance of Functor... I think.

5) `data D = D (Array Word Word) Int Int`

No. D is of kind `*`
