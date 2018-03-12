module Main where

import WordNumberTests
import QuickCheckTests

main :: IO ()
main = do
  runWordNumberTests
  runQuickCheckTests
