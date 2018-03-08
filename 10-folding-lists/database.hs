module Database where

import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate UTCTime
                  deriving (Eq, Ord, Show)

type Database = [DatabaseItem]

database :: Database
database = [
  DbDate (UTCTime
          (fromGregorian 1911 5 1)
          (secondsToDiffTime 34123)),
  DbNumber 9001,
  DbNumber 100,
  DbString "Hello, world!",
  DbDate (UTCTime
          (fromGregorian 1921 5 1)
          (secondsToDiffTime 34123))]

isDbString :: DatabaseItem -> Bool
isDbString (DbString _) = True
isDbString _ = False

isDbNumber :: DatabaseItem -> Bool
isDbNumber (DbNumber _) = True
isDbNumber _ = False

isDbDate :: DatabaseItem -> Bool
isDbDate (DbDate _) = True
isDbDate _ = False

filterDbDate :: Database -> [UTCTime]
filterDbDate = map extract . filter isDbDate
  where extract (DbDate date) = date

filterDbNumber :: Database -> [Integer]
filterDbNumber = map extract . filter isDbNumber
  where extract (DbNumber date) = date

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

avg :: (Fractional a, Foldable t) => t a -> a
avg xs = sum xs / fromIntegral (length xs)

avgDb :: [DatabaseItem] -> Double
avgDb = avg . map fromIntegral . filterDbNumber
