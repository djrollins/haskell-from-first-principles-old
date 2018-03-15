module FunctorInstances where

import FunctorProperties

import Test.QuickCheck
import Test.QuickCheck.Function

-- Identity a
newtype Identity a = Identity a
  deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

type IdentityCompose = Identity Int -> Fun Int Int -> Fun Int Int -> Bool

-- Pair a a
data Pair a = Pair a a
  deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)

instance (Arbitrary a) => Arbitrary (Pair a) where
  arbitrary = do
    (a, b) <- arbitrary
    return $ Pair a b

type PairCompose = Pair Char -> Fun Char Int -> Fun Int Bool -> Bool

-- Two a b c
data Two a b = Two a b
  deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance (Arbitrary a, Arbitrary b) =>
  Arbitrary (Two a b) where
    arbitrary = do
      (a, b) <- arbitrary
      return $ Two a b

type TwoCompose = Two Int Int -> Fun Int Char -> Fun Char Int -> Bool

-- Three a b c
data Three a b c = Three a b c
  deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
  Arbitrary (Three a b c) where
    arbitrary = do
      (a, b, c) <- arbitrary
      return $ Three a b c

type ThreeCompose = Three Int Int Int -> Fun Int Char -> Fun Char Int -> Bool

-- Three' a b
data Three' a b = Three' a b b
  deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)

instance (Arbitrary a, Arbitrary b) =>
  Arbitrary (Three' a b) where
    arbitrary = do
      (a, b, c) <- arbitrary
      return $ Three' a b c

type Three'Compose = Three' Int Int -> Fun Int Char -> Fun Char Int -> Bool

-- Four a b c d
data Four a b c d = Four a b c d
  deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
  Arbitrary (Four a b d c) where
    arbitrary = do
      (a, b, c, d) <- arbitrary
      return $ Four a b c d

type FourCompose = Four Int Int Int Int -> Fun Int Char -> Fun Char Int -> Bool

-- Four' a b
data Four' a b = Four' a a a b
  deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a b c d) = Four' a b c (f d)

instance (Arbitrary a, Arbitrary b) =>
  Arbitrary (Four' a b) where
    arbitrary = do
      (a, b, c, d) <- arbitrary
      return $ Four' a b c d

type Four'Compose = Four' Int Int -> Fun Int Char -> Fun Char Int -> Bool

data Trivial = Trivial

-- Functor Trivial not possible as Functor requires Kind * -> *, but Trivial is
-- of Kind *


runTests :: IO ()
runTests = do
  quickCheck $ (functorIdentity :: Identity Int -> Bool)
  quickCheck $ (functorCompose' :: IdentityCompose)
  quickCheck $ (functorIdentity :: Pair Int -> Bool)
  quickCheck $ (functorCompose' :: PairCompose)
  quickCheck $ (functorIdentity :: Four Int Int Int Int -> Bool)
  quickCheck $ (functorCompose' :: FourCompose)
  quickCheck $ (functorIdentity :: Four' Int Int -> Bool)
  quickCheck $ (functorCompose' :: Four'Compose)
  quickCheck $ (functorIdentity :: Three Int Int Int -> Bool)
  quickCheck $ (functorCompose' :: ThreeCompose)
  quickCheck $ (functorIdentity :: Three' Int Int -> Bool)
  quickCheck $ (functorCompose' :: Three'Compose)
  quickCheck $ (functorIdentity :: Two Int Int -> Bool)
  quickCheck $ (functorCompose' :: TwoCompose)
