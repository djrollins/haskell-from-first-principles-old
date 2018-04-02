module ChapterExercises where

import Control.Applicative (liftA3, liftA2)
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Foldable Identity where
  foldMap f (Identity a) = f a

instance Traversable Identity where
  traverse f (Identity x) = fmap Identity $ f x

newtype Constant a b = Constant { getConstant :: a }
  deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Monoid a => Foldable (Constant a) where
  foldMap f (Constant a) = mempty

instance Monoid a => Traversable (Constant a) where
  traverse _ (Constant a) = Constant <$> pure a

data Optional a = Nada | Yep a
  deriving (Eq, Ord, Show)

instance Functor Optional where
  fmap _ Nada = Nada
  fmap f (Yep a) = Yep $ f a

instance Foldable Optional where
  foldMap _ Nada = mempty
  foldMap f (Yep a) = f a

instance Traversable Optional where
  traverse f (Yep a) = Yep <$> f a
  traverse _ Nada = pure Nada

data List a = Nil | Cons a (List a)
  deriving (Eq, Ord, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons h t) = Cons (f h) (fmap f t)

instance Foldable List where
  foldMap f Nil = mempty
  foldMap f (Cons h t) = f h `mappend` foldMap f t

instance Traversable List where
  traverse f (Cons h t) = Cons <$> f h <*> traverse f t
  traverse _ Nil = pure Nil

data Three a b c = Three a b c
  deriving (Eq, Ord, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance Foldable (Three a b) where
  foldMap f (Three _ _ c) = f c

instance Traversable (Three a b) where
  traverse f (Three a b c) = Three a b <$> f c

data Pair a b = Pair a b
  deriving (Eq, Ord, Show)

instance Functor (Pair a) where
  fmap f (Pair x y) = Pair x (f y)

instance Foldable (Pair a) where
  foldMap f (Pair _ b) = f b

instance Traversable (Pair a) where
  traverse f (Pair a b) = Pair a <$> f b

data Big a b = Big a b b
  deriving (Eq, Ord, Show)

instance Functor (Big a) where
  fmap f (Big a b1 b2) = Big a (f b1) (f b2)

instance Foldable (Big a) where
  foldMap f (Big _ b1 b2) = f b1 `mappend` f b2

instance Traversable (Big a) where
  traverse f (Big a b1 b2) = Big a <$> f b1 <*> f b2

data Bigger a b = Bigger a b b b
  deriving (Eq, Ord, Show)

instance Functor (Bigger a) where
  fmap f (Bigger a b1 b2 b3) = Bigger a (f b1) (f b2) (f b3)

instance Foldable (Bigger a) where
  foldMap f (Bigger _ b1 b2 b3) = f b1 `mappend` f b2 `mappend` f b3

instance Traversable (Bigger a) where
  traverse f (Bigger a b1 b2 b3) = Bigger a <$> f b1 <*> f b2 <*> f b3

data S n a = S (n a) a
  deriving (Eq, Ord, Show)

instance Functor n => Functor (S n) where
  fmap f (S n a) = S (fmap f n) (f a)

instance Foldable n => Foldable (S n) where
  foldMap f (S n a) = foldMap f n `mappend` f a

instance Traversable n => Traversable (S n) where
  traverse f (S n a) = S <$> traverse f n <*> f a

data Tree a =
    Empty
  | Leaf a
  | Node (Tree a) a (Tree a)
  deriving (Eq, Ord, Show)

instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Leaf a) = Leaf $ f a
  fmap f (Node l a r) = Node (fmap f l) (f a) (fmap f r)

instance Foldable Tree where
  foldMap _ Empty = mempty
  foldMap f (Leaf a) = f a
  foldMap f (Node l a r) = foldMap f l `mappend` f a `mappend` foldMap f r

instance Traversable Tree where
  traverse _ Empty = pure Empty
  traverse f (Leaf a) = Leaf <$> f a
  traverse f (Node l a r) = liftA3 Node (traverse f l) (f a) (traverse f r)

-- Tests

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = fmap Identity arbitrary

instance Eq a => EqProp (Identity a) where (=-=) = eq

instance Arbitrary a => Arbitrary (Constant a b) where
  arbitrary = fmap Constant arbitrary

instance Eq a => EqProp (Constant a b) where (=-=) = eq

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = frequency [(3, Yep <$> arbitrary), (1, pure Nada)]
instance Eq a => EqProp (Optional a) where (=-=) = eq

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    h <- arbitrary
    t <- arbitrary
    frequency [(3, pure $ Cons h t), (1, pure Nil)]

instance Eq a => EqProp (List a) where (=-=) = eq

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = liftA3 Three arbitrary arbitrary arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
  arbitrary = liftA2 Pair arbitrary arbitrary

instance (Eq a, Eq b) => EqProp (Pair a b) where (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
  arbitrary = liftA3 Big arbitrary arbitrary arbitrary

instance (Eq a, Eq b) => EqProp (Big a b) where (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Bigger a b) where
  arbitrary = Bigger <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Bigger a b) where (=-=) = eq

instance (Functor n, Arbitrary (n a), Arbitrary a) => Arbitrary (S n a) where
  arbitrary = S <$> arbitrary <*> arbitrary

instance (Eq (n a), Eq a) => EqProp (S n a) where (=-=) = eq

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = frequency [
      (2, liftA3 Node arbitrary arbitrary arbitrary),
      (2, Leaf <$> arbitrary),
      (1, pure Empty)
    ]

instance Eq a => EqProp (Tree a) where (=-=) = eq

type Types = (String, String, [Bool])

main :: IO ()
main = do
  let identity :: Identity Types; identity = undefined
  let constant :: Constant Types Types; constant = undefined
  let optional :: Optional Types; optional = undefined
  let list :: List Types; list = undefined
  let three :: Three Types Types Types; three = undefined
  let pair :: Pair Types Types; pair = undefined
  let big :: Big Types Types; big = undefined
  let bigger :: Bigger Types Types; bigger = undefined
  let s :: S [] (Int, Int, [Int]); s = undefined
  let tree :: Tree Types; tree = undefined

  putStr "\nIdentity a:"
  quickBatch $ functor identity
  quickBatch $ traversable identity
  putStr "\nConstant a b:"
  quickBatch $ functor constant
  quickBatch $ traversable constant
  putStr "\nOptional a:"
  quickBatch $ functor optional
  quickBatch $ traversable optional
  putStr "\nList a:"
  quickBatch $ functor list
  quickBatch $ traversable list
  putStr "\nThree a b c:"
  quickBatch $ functor three
  quickBatch $ traversable three
  putStr "\nPair a b:"
  quickBatch $ functor pair
  quickBatch $ traversable pair
  putStr "\nBig a b:"
  quickBatch $ functor big
  quickBatch $ traversable big
  putStr "\nBigger a b:"
  quickBatch $ functor bigger
  quickBatch $ traversable bigger
  putStr "\nS n a:"
  quickBatch $ functor s
  quickBatch $ traversable s
  putStr "\nTree a:"
  quickBatch $ functor tree
  quickBatch $ traversable tree

