module Palindrome where

import Control.Monad (forever)
import System.Exit (exitSuccess)
import Data.Char (toLower, isAlpha)

stripNonAlpha :: String -> String
stripNonAlpha = map toLower . filter isAlpha

palindrome :: IO ()
palindrome = forever $ do
  line <- getLine

  let strippedLine = stripNonAlpha line

  if (strippedLine == reverse strippedLine)
    then
      putStrLn "It's a palindrome!"
    else do
      putStrLn "Nope!"
      exitSuccess
