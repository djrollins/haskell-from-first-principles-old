module ListApplicative where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Monoid (List a) where
  mempty = Nil
  mappend Nil ys = ys
  mappend (Cons x xs) ys = Cons x $ xs `mappend` ys

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons head rest) = Cons (f head) (fmap f rest)

instance Applicative List where
  pure x = Cons x Nil
  fs <*> xs = concatMap' (<$> xs) fs
  -- "for each f in fs, fmap it over xs, then concat the result"
  -- _ <*> Nil = Nil
  -- Nil <*> _ = Nil
  -- (Cons f fs) <*> xs = (f <$> xs) `mappend` (fs <*> xs)

fold :: (a -> b -> b) -> b -> List a -> b
fold _ acc Nil = acc
fold f acc (Cons h t) = f h (fold f acc t)

concat' :: List (List a) -> List a
concat' = fold mappend mempty

concatMap' :: (a -> List b) -> List a -> List b
concatMap' f = concat' . fmap f

take' :: Int -> List a -> List a
take' n lst = go n lst
  where
    go 0 _   = Nil
    go _ Nil = Nil
    go n (Cons x xs) = Cons x (go (n - 1) xs)

zipWith' :: (a -> b -> c) -> List a -> List b -> List c
zipWith' f (Cons x xs) (Cons y ys) = Cons (f x y) $ zipWith' f xs ys
zipWith' _ _ _                     = Nil

repeat' :: a -> List a
repeat' x = Cons x $ repeat' x

newtype ZipList a =
  ZipList (List a)
  deriving (Eq, Show)

instance Functor ZipList where
  fmap f (ZipList xs) = ZipList $ fmap f xs

instance Applicative ZipList where
  pure = ZipList . repeat'
  ZipList fs <*> ZipList xs = ZipList $ zipWith' ($) fs xs

-- QuickCheck instances
instance Eq a => EqProp (List a) where
  (=-=) = eq

instance Eq a => EqProp (ZipList a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList l) = xs in take' 300 l
          ys' = let (ZipList l) = ys in take' 300 l

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = frequency [
      (3, Cons <$> arbitrary <*> arbitrary),
      (1, return $ Nil)
    ]

instance Arbitrary a => Arbitrary (ZipList a) where
  arbitrary = ZipList <$> arbitrary

runTests :: IO ()
runTests = do
  quickBatch $ monoid (undefined :: (List (String, Int, Char)))
  quickBatch $ functor (undefined :: (List (String, Int, Char)))
  quickBatch $ applicative (undefined :: (List (String, Int, Char)))
  quickBatch $ functor (undefined :: (ZipList (String, Int, Char)))
  quickBatch $ applicative (undefined :: (ZipList (String, Int, Char)))
