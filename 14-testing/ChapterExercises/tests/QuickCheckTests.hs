module QuickCheckTests where

import Data.List (sort)
import Test.QuickCheck

half :: Fractional a => a -> a
half = (/2)

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
  snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status
        go y (Nothing, t) = (Just y, t)
        go y (Just x, _) = (Just y, x >= y)

prop_doubleHalfXisX :: Double -> Bool
prop_doubleHalfXisX x = 2 * half x == x

prop_sortedList :: [Int] -> Bool
prop_sortedList = listOrdered . sort

isAssociative :: (Num a, Eq a) => (a -> a -> a) -> a -> a -> a -> Bool
isAssociative f x y z = f x (f y z) == f (f x y) z

isCommutative :: (Num a, Eq a) => (a -> a -> a) -> a -> a -> Bool
isCommutative f x y = (f x y) == (f y x)

prop_plusAssociative :: Integer -> Integer -> Integer -> Bool
prop_plusAssociative = isAssociative (+)

prop_plusCommutative :: Integer -> Integer -> Bool
prop_plusCommutative = isCommutative (+)

prop_multAssociative :: Integer -> Integer -> Integer -> Bool
prop_multAssociative = isAssociative (*)

prop_multCommutative :: Integer -> Integer -> Bool
prop_multCommutative = isCommutative (*)

prop_quotRemLaw :: Integer -> Integer -> Bool
prop_quotRemLaw x y = (quot x y)*y + (rem x y) == x

prop_divModLaw :: Integer -> Integer -> Bool
prop_divModLaw x y = (div x y)*y + (mod x y) == x

prop_powAssociative :: Integer -> Integer -> Integer -> Bool
prop_powAssociative = isAssociative (^)

prop_powCommutative :: Integer -> Integer -> Bool
prop_powCommutative = isCommutative (^)

genNonZeroPair :: Gen (Integer, Integer)
genNonZeroPair = do
  x <- arbitrary `suchThat` (/=0)
  y <- arbitrary `suchThat` (/=0)
  return (x, y)

prop_ReverseReverseIsId :: [Integer] -> Bool
prop_ReverseReverseIsId xs = (reverse . reverse) xs == id xs

prop_dollar :: (Eq b) => (Integer -> b) -> Integer -> Bool
prop_dollar f a = (f $ a) == (f a)

prop_composition :: (Eq b) => (a -> b) -> (Integer -> a) -> Integer -> Bool
prop_composition f g a = (f . g $ a) == ((\x -> f (g x)) a)

prop_foldPlusPlus :: [Integer] -> Bool
prop_foldPlusPlus xs = (foldr (:) [] xs) == [] ++ xs

data Fool = Fulse | Frue deriving (Eq, Show)

genEvenFool :: Gen Fool
genEvenFool = oneof [return Fulse, return Frue]

genMostlyFulse :: Gen Fool
genMostlyFulse = frequency [(2, return Fulse), (1, return Frue)]

runQuickCheckTests :: IO ()
runQuickCheckTests = do
  quickCheck prop_doubleHalfXisX
  quickCheck prop_sortedList
  quickCheck prop_plusAssociative
  quickCheck prop_plusCommutative
  quickCheck prop_multCommutative
  quickCheck prop_multCommutative
  --quickCheck prop_powAssociative
  --quickCheck prop_powCommutative
  quickCheck $ forAll genNonZeroPair $ uncurry prop_quotRemLaw
  quickCheck prop_ReverseReverseIsId
  quickCheck $ prop_dollar negate
  quickCheck $ prop_composition negate negate
  quickCheck $ prop_foldPlusPlus

