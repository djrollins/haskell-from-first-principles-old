module Validations where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Validation e a =
    Failure' e
  | Success' a
  deriving (Eq, Show)

instance Functor (Validation a) where
  fmap f (Success' a) = Success' $ f a
  fmap _ (Failure' a) = Failure' a

instance Monoid e => Applicative (Validation e) where
  pure = Success'
  (Success' f) <*> (Success' a) = Success' $ f a
  (Failure' e1) <*> (Failure' e2) = Failure' $ e1 `mappend` e2
  (Failure' e) <*> _ = Failure' e
  _ <*> (Failure' e) = Failure' e

-- Tests

instance (Eq a, Eq e) => EqProp (Validation e a) where (=-=) = eq

instance (Arbitrary a, Arbitrary e) => Arbitrary (Validation e a) where
  arbitrary = oneof [Success' <$> arbitrary, Failure' <$> arbitrary]

runTests :: IO ()
runTests = do
  quickBatch $ functor (undefined :: Validation String (Int, Char, Bool))
  quickBatch $ applicative (undefined :: Validation String (Int, Char, Bool))
