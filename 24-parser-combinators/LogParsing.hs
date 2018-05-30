{-# LANGUAGE QuasiQuotes #-}
module LogParsing where

import qualified Data.Text as T
import Data.List (intercalate)
import Control.Applicative
import Control.Monad
import Text.Trifecta
import Text.RawString.QQ

type Hours = Int
type Minutes = Int
type Year = Int
type Month = Int
type Day = Int

data Time = Time Hours Minutes
data Date = Date Year Month Day
data Entry = Entry Time String
data DayLog = DayLog Date [Entry]

newtype Log = Log [DayLog]

showPad :: Int -> String
showPad n
  | length nStr == 1 = '0' : nStr
  | otherwise = nStr
  where nStr = show n

trim :: String -> String
trim = T.unpack . T.strip . T.pack

instance Show Time where
  show (Time hrs mins) = showPad hrs ++ ":" ++ showPad mins

instance Show Date where
  show (Date year month day) = "# " ++ (intercalate "-" [show year, showPad month, showPad day])

instance Show Entry where
  show (Entry time activity) = unwords [show time, activity]

instance Show DayLog where
  show (DayLog date entries) = unlines $ show date : (fmap show entries)

instance Show Log where
  show (Log log) = trim . unlines $ fmap show log

int :: Parser Int
int = fromIntegral <$> integer

parseDate :: Parser Date
parseDate = Date <$> (char '#' *> spaces *> int)
                 <*> (char '-' *> int)
                 <*> (char '-' *> int)

parseTime :: Parser Time
parseTime = Time <$> (int <* char ':') <*> int

parseEntry :: Parser Entry
parseEntry = do
  time <- parseTime
  rest <- manyTill anyChar (void (try parseComment <|> string "\n" <|> (eof >> return "")))
  spaces
  return $ Entry time (trim rest)

parseDayLog :: Parser DayLog
parseDayLog = do
  parsePreamble
  date <- parseDate <* (skipOptional parseComment <|> spaces)
  entries <- many parseEntry
  parsePreamble
  return $ DayLog date entries

parseLog :: Parser Log
parseLog = Log <$> many parseDayLog

parseComment :: Parser String
parseComment = do
  spaces
  start <- string "--"
  content <- manyTill anyChar (void (char '\n') <|> void (eof))
  return $ start ++ content

parsePreamble :: Parser [String]
parsePreamble = many (try parseComment) <* spaces

-- "tests"

testParseComment = parseComment <* eof
testParsePreamble = parsePreamble <* eof
testParseDate = parseDate <* eof
testParseTime = parseTime <* eof
testParseEntry = parseEntry <* eof
testParseDayLog = parseDayLog <* eof
testParseLog = parseLog <* eof

entry = "12:34 foo bar baz"
entryWithComment = "12:34 quux -- doot doot"
date = "# 2018-05-08"
time = "14:42"
comment = "-- foo bar baz \n"
preamble = [r|

-- foo
   --

|]

dayLog = [r|
--comment
# 2025-02-07 -- dates not nececessarily sequential
08:00 Breakfast -- should I try skippin bfast?
09:00 Bumped head, passed out
|]

fullLog = [r|

-- wheee a comment

# 2025-02-05
08:00 Breakfast
09:00 Sanitizing moisture collector
11:00 Exercising in high-grav gym
12:00 Lunch
13:00 Programming
17:00 Commuting home in rover
17:30 R&R
19:00 Dinner
21:00 Shower
21:15 Read
22:00 Sleep


# 2025-02-07 -- dates not nececessarily sequential
08:00 Breakfast -- should I try skippin bfast?
09:00 Bumped head, passed out
13:36 Wake up, headache
13:37 Go to medbay
13:40 Patch self up
13:45 Commute home for rest
14:15 Read
21:00 Dinner
21:15 Read
22:00 Sleep

|]

main = do
  --print $ parseString testParseComment mempty comment
  --print $ parseString testParsePreamble mempty preamble
  --print $ parseString testParseDate mempty date
  --print $ parseString testParseTime mempty time
  --print $ parseString testParseEntry mempty entry
  --print $ parseString testParseEntry mempty entryWithComment
  --print $ parseString testParseDayLog mempty dayLog
  print $ parseString testParseLog mempty fullLog
