module Person where

import System.IO (hSetBuffering, stdout, BufferMode (NoBuffering))
import Text.Read (readMaybe)

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid =
    NameEmpty
  | AgeTooLow
  | PersonInvalidUnknown String
  deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 = Right $ Person name age
  | name == "" = Left NameEmpty
  | age  <= 0 = Left AgeTooLow
  | otherwise =
    Left $ PersonInvalidUnknown $
      "Name was: " ++ show name ++
      " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
  putStr "Enter your name: "
  name <- getLine
  putStr "Enter your age: "
  ageStr <- getLine

  case readMaybe ageStr of
    Just age -> putStrLn . show $ mkPerson name age
    Nothing -> putStrLn "Invalid age"
