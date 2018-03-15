module Possibly where

import FunctorProperties
import Test.QuickCheck
import Test.QuickCheck.Function

data Possibly a = LolNope | Yeppers a
  deriving (Eq, Show)

instance Functor Possibly where
  fmap f (Yeppers a) = Yeppers $ f a
  fmap _ LolNope     = LolNope

instance Arbitrary a => Arbitrary (Possibly a) where
  arbitrary = do
    a <- arbitrary
    oneof [return $ Yeppers a, return $ LolNope]

type PossiblyCompose = Possibly Int -> Fun Int Int -> Fun Int Int -> Bool

runTests :: IO ()
runTests = do
  quickCheck $ (functorIdentity :: Possibly Int -> Bool)
  quickCheck $ (functorCompose' :: PossiblyCompose)
