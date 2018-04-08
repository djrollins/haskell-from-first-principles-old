module FizzBuzz where

import Control.Monad
import Control.Monad.Trans.State
import qualified Data.DList as DL

fizzBuzz :: Integer -> String
fizzBuzz n
  | n `mod` 15 == 0 = "FizzBuzz"
  | n `mod`  5 == 0 = "Buzz"
  | n `mod`  3 == 0 = "Fizz"
  | otherwise       = show n

fizzBuzzList :: [Integer] -> DL.DList String
fizzBuzzList list =
  execState (mapM_ addResult list) DL.empty

addResult :: Integer -> State (DL.DList String) ()
addResult n = do
  xs <- get
  let result = fizzBuzz n
  -- append to xs... inverse of cons... snoc... get it?
  put (DL.snoc xs result)

fizzBuzzFromTo :: Integer -> Integer -> [String]
fizzBuzzFromTo x y = DL.apply (fizzBuzzList $ enumFromTo x y) []

main :: IO ()
main = mapM_ putStrLn $ fizzBuzzFromTo 4 25
