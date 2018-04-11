module PositiveIntegers where

import Text.Trifecta

parseDigit :: Parser Char
parseDigit = oneOf ['0'..'9'] <?> "digit"

base10Integer :: Parser Integer
base10Integer = do
  sign <- optional $ char '-'
  value <- read <$> some parseDigit
  case sign of
    Nothing -> return $ value
    _       -> return $ negate value
