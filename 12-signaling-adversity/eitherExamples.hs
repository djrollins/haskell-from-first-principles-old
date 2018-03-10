module EitherExamples where

import Control.Applicative

type Name = String
type Age = Integer
type ValidatePerson a = Either [PersonInvalid] a

data Person = Person Name Age deriving (Eq, Show)
data PersonInvalid = NameEmpty | AgeToLow deriving (Eq, Show)

mkPersonMaybe :: Name -> Age -> Maybe Person
mkPersonMaybe name age
  | name /= "" && age >= 0 = Just $ Person name age
  | otherwise = Nothing

mkPersonEither :: Name -> Age -> Either PersonInvalid Person
mkPersonEither name age
  | name == "" = Left NameEmpty
  | age < 0 = Left AgeToLow
  | otherwise = Right $ Person name age

ageOkay :: Age -> Either [PersonInvalid] Age
ageOkay age
  | age >= 0 = Right age
  | otherwise = Left [AgeToLow]

nameOkay :: Name -> Either [PersonInvalid] Name
nameOkay name
  | name /= "" = Right name
  | otherwise = Left [NameEmpty]

mkPersonValidated' :: ValidatePerson Name
                   -> ValidatePerson Age
                   -> ValidatePerson Person

mkPersonValidated' (Right nameOk) (Right ageOk) = Right (Person nameOk ageOk)
mkPersonValidated' (Left nameBad) (Left ageBad) = Left (nameBad ++ ageBad)
mkPersonValidated' (Left nameBad) (Right ageOk) = Left nameBad
mkPersonValidated' (Right nameOk) (Left ageBad) = Left ageBad

mkPersonValidated :: Name -> Age -> ValidatePerson Person
mkPersonValidated name age = mkPersonValidated' (nameOkay name) (ageOkay age)

mkPersonLifted :: Name -> Age -> Either [PersonInvalid] Person
mkPersonLifted name age = liftA2 Person (nameOkay name) (ageOkay age)
