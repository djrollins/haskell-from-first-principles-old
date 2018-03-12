module Main where

import qualified Data.Map as M
import Morse
import Test.QuickCheck

allowedChars :: [Char]
allowedChars = M.keys letterToMorse

allowedMorse :: [Morse]
allowedMorse = M.elems letterToMorse

charGen :: Gen Char
charGen = elements allowedChars

morseGen :: Gen Morse
morseGen = elements allowedMorse

prop_thereAndBackAgain :: Property
prop_thereAndBackAgain =
  forAll charGen (\c -> ((charToMorse c) >>= morseToChar) == Just c)


data ComplexType a b c = Product1 a b | Product2 c deriving (Show)

getComplex :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (ComplexType a b c)
getComplex = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  oneof [return $ Product1 a b, return $ Product2 c]

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
         Arbitrary (ComplexType a b c)  where
  arbitrary = getComplex

main :: IO ()
main = quickCheck prop_thereAndBackAgain
