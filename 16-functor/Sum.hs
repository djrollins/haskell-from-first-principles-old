module Sum where

import FunctorProperties
import Test.QuickCheck
import Test.QuickCheck.Function

data Sum a b = First a | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap f (Second b) = Second $ f b
  fmap _ (First a) = First a

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    oneof [return $ First a, return $ Second b]

type SumCompose = Sum String String -> Fun String String -> Fun String Int -> Bool

runTests :: IO ()
runTests = do
  quickCheck $ (functorIdentity :: Sum String Int -> Bool)
  quickCheck $ (functorCompose' :: SumCompose)
