module Scans where

fibs = 1 : scanl (+) 1 fibs

fibs20 = take 20 fibs

fibsLess100 = takeWhile (<100) fibs

factorial = scanl (*) 1 [1..]

