module ChapterExercises where

import Control.Applicative
import Data.Monoid

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

{-
  1. []

    pure :: a -> [a]
    (<*>) :: [(a -> b)] -> [a] -> [b]

  2. IO

    pure :: a -> IO a
    (<*>) :: IO (a -> b) -> IO a -> IO b

  3. (,) a

    pure :: Monoid a => b -> (a, b)
    (<*>) :: Monoid a => (a, b -> c) -> (a, b) -> (a, c)

  4. (->) e = EFunc

    pure :: a -> EFunc a
    pure :: a -> (e -> a)

    (<*>) :: EFunc (a -> b) -> EFunc a -> EFunc b
    (<*>) :: (e -> (a -> b)) -> (e -> a) -> (e -> b)
 -}

data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative Pair where
  pure x = Pair x x
  (Pair f g) <*> (Pair x y) = Pair (f x) (g y)

data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance Monoid a => Applicative (Two a) where
  pure b = Two mempty b
  (Two a1 f) <*> (Two a2 x) = Two (a1 <> a2) (f x)

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure c = Three mempty mempty c
  (Three a1 b1 f) <*> (Three a2 b2 x) = Three (a1 <> a2) (b1 <> b2) (f x)

data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b1 b2) = Three' a (f b1) (f b2)

instance Monoid a => Applicative (Three' a) where
  pure x = Three' mempty x x
  (Three' a1 f g) <*> (Three' a2 x y) = Three' (a1 <> a2) (f x) (g y)

-- data Four ignore because it's more of the same

data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a1 a2 a3 b) = Four' a1 a2 a3 (f b)

instance Monoid a => Applicative (Four' a) where
  pure x = Four' mempty mempty mempty x
  (Four' a1 a2 a3 f) <*> (Four' a1' a2' a3' x) =
    Four' (a1 <> a1') (a2 <> a2') (a3 <> a3') (f x)

-- Tests

instance Arbitrary a => Arbitrary (Pair a) where
--arbitrary = Pair <$> arbitrary <*> arbitrary
  arbitrary = liftA2 Pair arbitrary arbitrary

instance Eq a => EqProp (Pair a) where (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = liftA2 Two arbitrary arbitrary

instance (Eq a, Eq b) => EqProp (Two a b) where (=-=) = eq

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
  Arbitrary (Three a b c) where
    arbitrary = liftA3 Three arbitrary arbitrary arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = liftA3 Three' arbitrary arbitrary arbitrary

instance (Eq a, Eq b) => EqProp (Three' a b) where (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = Four' <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Four' a b) where (=-=) = eq

type S = String

runTests :: IO ()
runTests = do
  quickBatch $ functor (undefined :: Pair (Int, Char, S))
  quickBatch $ applicative (undefined :: Pair (Int, Char, S))
  quickBatch $ functor (undefined :: Two (S, S, S) (Int, Char, S))
  quickBatch $ applicative (undefined :: Two (S, S, S) (Int, Char, S))
  quickBatch $ functor (undefined :: Three (S, S, S) (S, S, S) (S, S, S))
  quickBatch $ applicative (undefined :: Three (S, S, S) (S, S, S) (S, S, S))
  quickBatch $ functor (undefined :: Three' (S, S, S) (Int, Char, S))
  quickBatch $ applicative (undefined :: Three' (S, S, S) (Int, Char, S))
  quickBatch $ functor (undefined :: Four' (S, S, S) (Int, Char, S))
  quickBatch $ applicative (undefined :: Four' (S, S, S) (Int, Char, S))


-- vowels and stops

stops = "pbtdkg"
vowels = "aeiou"
third = [1, 2, 3]

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 (,,)
