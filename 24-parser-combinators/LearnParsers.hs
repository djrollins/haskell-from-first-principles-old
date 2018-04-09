module LearnParsers where

import Text.Trifecta
import Text.Parser.Combinators

stop :: Parser a
stop = unexpected "stop"

one = char '1'
one' = one >> stop

two = char '2'

oneTwo = one >> two
oneTwo' = oneTwo >> eof

three = char '3'

oneTwoThree :: Parser String
oneTwoThree = choice [string "123", string "12", string "1"]

oneTwoThreeChar :: Parser Char
oneTwoThreeChar = choice [
    one >> two >> three,
    one >> two,
    one
  ]


testParse :: Parser Char -> IO ()
testParse p = print $ parseString p mempty "123"

testParseEOF :: Parser () -> IO ()
testParseEOF p =
  print $ parseString p mempty "123"

testParseString :: Parser String -> IO ()
testParseString p =
  print $ parseString p mempty "12"

pNL s = putStrLn ('\n' : s)

main = do
  pNL "stop:"
  testParse stop
  pNL "one:"
  testParse one
  pNL "one':"
  testParse one'
  pNL "oneTwo:"
  testParse oneTwo
  pNL "oneTwo':"
  testParseEOF oneTwo'
  pNL "oneTwoThree:"
  testParseString oneTwoThree
  pNL "oneTwoThreeChar:"
  testParse oneTwoThreeChar
