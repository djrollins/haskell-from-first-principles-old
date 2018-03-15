module ChapterExercises where

import Data.Semigroup

data Trivial = Trivial
  deriving (Show, Eq)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Monoid Trivial where
  mempty = Trivial
  mappend = (<>)

newtype Identity a = Identity a deriving (Show, Eq)

instance Semigroup a => Semigroup (Identity a) where
  (Identity a) <> (Identity b) = Identity $ a <> b

instance (Semigroup a, Monoid a) => Monoid (Identity a) where
  mempty = Identity $ mempty
  mappend = (<>)

data Two a b = Two a b deriving (Show, Eq)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two a b) <> (Two a' b') = Two (a <> a') (b <> b')

instance (Monoid a, Semigroup a, Monoid b, Semigroup b) => Monoid (Two a b) where
  mempty = Two mempty mempty
  mappend = (<>)

data Three a b c = Three a b c deriving (Show, Eq)

instance (Semigroup a, Semigroup b, Semigroup c) =>
  Semigroup (Three a b c) where
    (Three a b c) <> (Three a' b' c') = Three (a <> a') (b <> b') (c <> c')

data Four a b c d = Four a b c d deriving (Show, Eq)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) =>
  Semigroup (Four a b c d) where
    (Four a b c d) <> (Four a' b' c' d') =
      Four (a <> a') (b <> b') (c <> c') (d <> d')

newtype BoolConj = BoolConj Bool deriving (Show, Eq)

instance Semigroup BoolConj where
  (BoolConj True) <> (BoolConj True) = BoolConj True
  _ <> _                             = BoolConj False

instance Monoid BoolConj  where
  mempty = BoolConj True
  mappend = (<>)

newtype BoolDisj = BoolDisj Bool deriving (Show, Eq)

instance Semigroup BoolDisj where
  (BoolDisj True) <> _ = BoolDisj True
  _ <> (BoolDisj True) = BoolDisj True
  _ <> _               = BoolDisj False

data Or a b = Fst a | Snd b deriving (Show, Eq)

instance Semigroup (Or a b) where
  (Snd a) <> _ = Snd a
  _       <> a = a

newtype Combine a b = Combine { unCombine :: (a -> b) }

instance Semigroup b => Semigroup (Combine a b) where
  Combine f <> Combine g = Combine $ f <> g

instance (Semigroup b, Monoid b) => Monoid (Combine a b) where
  mempty = Combine $ \_ -> mempty
  mappend = (<>)

newtype Comp a = Comp { unComp :: (a -> a) }

instance Semigroup a => Semigroup (Comp a) where
  Comp f <> Comp g = Comp $ f . g

instance (Semigroup a, Monoid a) => Monoid (Comp a) where
  mempty = Comp id
  mappend = (<>)

data Validation a b =
  Failure' a | Success' b
  deriving (Eq, Show)

instance Semigroup (Validation a b) where
  s@(Success' _) <> _ = s
  _ <> v             = v

newtype Mem s a = Mem { runMem :: s -> (a, s) }


combineMemFuncs f g x =
  let
    (a, b) = f x
    (c, d) = g b
  in
    (a <> c, d)

instance (Semigroup a) => Semigroup (Mem s a) where
  (Mem f) <> (Mem g) = Mem $ combineMemFuncs f g

instance (Semigroup a, Monoid a) => Monoid (Mem s a) where
  mempty = Mem $ \s -> (mempty, s)
  mappend = (<>)
