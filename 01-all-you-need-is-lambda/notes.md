# Domain, Co-domain and Image

Given the function signature:
    Int -> Bool

The Domain is the set of all Integers.
The Co-domain is the set {True, False}.

Given the implementation:
    x -> True

The Image is the set {True}.

So the domain and co-domain are defined by the signature, whilst the image is
defined by the implementation... I think.

# Head, parameters and bodies of a lambda

    \ x . x
    ^─┬─^
      └────── extent of the head of the lambda.
    \ x . x
      ^────── the single parameter of the
              function. This binds any
              variables with the same name
              in the body of the function.
    \ x . x
          ^── body, the expression the lambda
              returns when applied. This is a
              bound variable.

    \x . x y
         ^─^─ y is a free variable, whereas x is a bound variable

# alpha-equivalence only applies to bound variables

    \x.xz  & \x.xy  : not equivalent
    \xy.yx & \ab.ba : are equivalent
    \x.xz  & \y.yz  : are equivalent (the free var (z) is the same in both bodies)

# Example of complex beta reduction

    (\xyz.xz(yz))(\mn.m)(\p.p)       -> make currying explicit
    (\x.\y.\z.xz(yz))(\m.\n.m)(\p.p) -> [x := (\m.\n.m)]
    (\y.\z.(\m.\n.m)z(yz))(\p.p)     -> [y := (\p.p)]
    (\z.(\m.\n.m)z)((\p.p)z)          -> [m := z]
    (\z.(\n.z))((\p.p)z)             -> [n := ((\p.p)z)] : n doesn't appear in body so disappears
    \z.z

# Equivalence exercises

    1) \xy.xz  -> \mn.mz
    2) \xy.xxy -> \a.(\b.aab)
    3) \xyz.zx -> \tos.st

# Combinators

Combinators are functions where there are no free variables. They exist solely
to combine the arguments

    \xy.yxy is a combinator
    \xy.xyz is not a combinator

# Divergence

When beta-reduction actually does not reduce the expression:

    (\x.xx)(\y.yy) -> [x := \y.yy]
    (\y.yy)(\y.yy) -> [y := \y.yy]
    (\y.yy)(\y.yy) -> [y := \y.yy]
    (\y.yy)(\y.yy) -> [y := \y.yy]
    ...
