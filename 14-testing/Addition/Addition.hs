module Addition where

import Test.Hspec
import Test.QuickCheck

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

main :: IO ()
main = hspec $ do
  describe "Addition" $ do
    it "adds 1 and 1 to produce 4" $ do
      (1 + 1 :: Int) `shouldBe` 2

    it "adds 2 and 2 to equal 4" $ do
      (2 + 2 :: Int) `shouldBe` 4

    it "x + 1 is always greater than x" $ do
      property $ \x -> x + 1 > (x :: Int)

  describe "Sum" $ do
    it "sums all elements in a list" $ do
      sum' ([1, 2, 3, 4] :: [Int])  `shouldBe` 10
