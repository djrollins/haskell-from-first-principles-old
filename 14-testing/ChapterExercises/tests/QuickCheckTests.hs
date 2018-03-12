module QuickCheckTests where

import Data.List (sort)
import Test.QuickCheck
import Test.Hspec

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

prop_plusAssociative :: Integer -> Integer -> Integer -> Bool
prop_plusAssociative = isAssociative (+)

prop_plusCommutative :: Integer -> Integer -> Bool
prop_plusCommutative x y = x + y == y + x

runQuickCheckTests :: IO ()
runQuickCheckTests = do
  quickCheck prop_doubleHalfXisX
  quickCheck prop_sortedList
  quickCheck prop_plusAssociative
  quickCheck prop_plusCommutative
