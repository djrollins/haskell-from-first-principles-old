{-# LANGUAGE OverloadedStrings #-}
module TryTry where

import Control.Applicative ((<|>))
import Data.Ratio ((%))
import Text.Trifecta

badFraction = "1/0"
alsoBad = "10"
shouldWork = "1/2"
shouldAlsoWork = "2/1"

parseFraction :: Parser Rational
parseFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "Denominator cannot be zero"
    _ -> return (numerator % denominator)

type FractionOrDecimal =
  Either Rational Integer

parseFractionOrDecimal :: Parser FractionOrDecimal
parseFractionOrDecimal = (Left <$> try parseFraction) <|> (Right <$> decimal)

main :: IO ()
main = do
  let parseFraction' = parseString parseFraction mempty
  print $ parseString parseFractionOrDecimal mempty "12/2"
  print $ parseString parseFractionOrDecimal mempty "12"
