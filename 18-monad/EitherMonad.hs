module EitherMonad where

import Control.Monad (ap)

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Sum a b = First a | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap f (Second b) = Second $ f b
  fmap f (First a)  = First a

instance Applicative (Sum a) where
  pure = Second
  (<*>) = ap

instance Monad (Sum a) where
  return = pure
  (Second b) >>= f = f b
  (First a) >>= _ = First a


-- Tests
instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = oneof [First <$> arbitrary, Second <$> arbitrary]

instance (Eq a, Eq b) => EqProp (Sum a b) where
  (=-=) = eq

runTests :: IO ()
runTests = do
  let types :: Sum (Int, Char, String) (Int, Char, String)
      types = undefined

  quickBatch $ functor types
  quickBatch $ applicative types
  quickBatch $ monad types
