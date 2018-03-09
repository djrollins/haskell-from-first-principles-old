module Ciphers where

import Data.Char

encodeChar :: Char -> Char -> Char
encodeChar keywordChar textChar = chr ((((ord textChar) + (ord keywordChar) - ordA * 2) `mod` 26) + ordA)
  where ordA = ord 'A'

repeatString :: String -> String
repeatString = concat . repeat

vigenere :: String -> String -> String
vigenere keyword text = go (repeatString keyword) text
  where go _ [] = []
        go kw@(k:ks) (c:cs)
         | isSpace c = c : go kw cs
         | otherwise = (encodeChar k c) : go ks cs
