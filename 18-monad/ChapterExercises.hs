module ChapterExercises where

import Control.Monad (ap)
import Control.Applicative (liftA2)

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- 1) Nope

data Nope a = NopeDotJpg
  deriving Show

instance Functor Nope where
  fmap _ _ = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  _ <*> _ = NopeDotJpg

instance Monad Nope where
  return = pure
  _ >>= _ = NopeDotJpg

-- PhhhbbttEither b a
data PhhhbbttEither b a =
  Left' a | Right' b
  deriving (Eq, Show)

instance Functor (PhhhbbttEither b) where
  fmap _ (Right' b) = Right' b
  fmap f (Left' a) = Left' $ f a

instance Applicative (PhhhbbttEither b) where
  pure = Left'
  (<*>) = ap

instance Monad (PhhhbbttEither b) where
  return = pure
  (Right' b) >>= _ = Right' b
  (Left' a)  >>= f = f a

-- Identity
data Identity a = Identity a
  deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
  pure = Identity
  (<*>) = ap

instance Monad Identity where
  return = pure
  (Identity a) >>= f = f a

-- List a
data List a = Nil | Cons a (List a)
  deriving (Eq, Show)

fold :: (a -> b -> b) -> b -> List a -> b
fold _ acc Nil = acc
fold f acc (Cons h t) = f h (fold f acc t)

concatList :: List (List a) -> List a
concatList = fold mappend mempty

instance Monoid (List a) where
  mempty = Nil
  mappend Nil ys = ys
  mappend (Cons x xs) ys = Cons x $ xs `mappend` ys

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons h t) = Cons (f h) (fmap f t)

instance Applicative List where
  pure x = Cons x Nil
  (<*>) = ap

instance Monad List where
  return = pure
  lst >>= f = concatList $ f <$> lst

j :: Monad m => m (m a) -> m a
j = flip (>>=) id

l1 :: Monad m => (a -> b) -> m a -> m b
-- l1 = fmap
l1 f m = do { a <- m; return $ f a }

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f ma mb = do
  a <- ma
  b <- mb
  return $ f a b

a :: Monad m => m a -> m (a -> b) -> m b
-- a = flip (<*>)
a ma mf = do
  a <- ma
  f <- mf
  return $ f a

{-
  How sequence' works:

  Imagining Monad m is Maybe; this works because if x turns out to be Nothing,
  `x >>= \x' -> fmap (x':) (sequence' xs)` will also produce Nothing because
  `Nothing >>= anything` (which is what the do-notation desugars into)
  always short-circuits to Nothing.

  Then, if the next element is `Just`, the the state of the computation becomes
  `(Just x') >>= \x' -> fmap (x':) Nothing`. As fmapping over Nothing always
  gives Nothing, the Nothing will end up propogating all the way to the end of
  the computation.

  In the case where all elements in [m a] are Justs, then it recurses all the
  way down to `sequence [] = Just []` which means x' can be Cons'ed into the
  with fmap.
-}
sequence' :: Monad m => [m a] -> m [a]
sequence' [] = return []
sequence' (x:xs) = do
  x' <- x
  fmap (x':) (sequence' xs)

meh :: Monad m => [a] -> (a -> m b) -> m [b]
-- meh as f = sequence $ fmap f as
meh as f = sequence' $ fmap f as

flipType :: Monad m => [m a] -> m [a]
flipType as = meh as id

-- Tests

type TestTypes = (Int, Char, String)

runTests :: IO ()
runTests = do

  putStrLn "\nNope a:"
  quickBatch $ functor (undefined :: Nope TestTypes)
  quickBatch $ applicative (undefined :: Nope TestTypes)
  quickBatch $ monad (undefined :: Nope TestTypes)

  putStrLn "\nPhhhbbttEither b a:"
  quickBatch $ functor (undefined :: PhhhbbttEither TestTypes TestTypes)
  quickBatch $ applicative (undefined :: PhhhbbttEither TestTypes TestTypes)
  quickBatch $ monad (undefined :: PhhhbbttEither TestTypes TestTypes)

  putStrLn "\nIdentity a:"
  quickBatch $ functor (undefined :: Identity TestTypes)
  quickBatch $ applicative (undefined :: Identity TestTypes)
  quickBatch $ monad (undefined :: Identity TestTypes)

  putStrLn "\nList a:"
  quickBatch $ functor (undefined :: List TestTypes)
  quickBatch $ applicative (undefined :: List TestTypes)
  quickBatch $ monad (undefined :: List TestTypes)

-- Test Support

instance Arbitrary (Nope a) where
  arbitrary = return $ NopeDotJpg

instance EqProp (Nope a) where
  _ =-= _ = property (True)

instance (Arbitrary a, Arbitrary b) => Arbitrary (PhhhbbttEither a b) where
  arbitrary = oneof [Left' <$> arbitrary, Right' <$> arbitrary]

instance (Eq a, Eq b) => EqProp (PhhhbbttEither b a) where (=-=) = eq

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where (=-=) = eq

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = frequency [(3, liftA2 Cons arbitrary arbitrary), (1, return $ Nil)]

instance Eq a => EqProp (List a) where (=-=) = eq
