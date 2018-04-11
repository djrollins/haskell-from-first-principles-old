-- Oh My! British phone numbers are insane. I think I'll still to US.
module PhoneNumbers where

import Text.Trifecta
import Control.Applicative

type NumberingPlanArea = Int
type Exchange = Int
type LineNumber = Int

data PhoneNumber = PhoneNumber NumberingPlanArea Exchange LineNumber
  deriving (Eq, Show)

parsePhone :: Parser PhoneNumber
parsePhone = do
  _ <- optional (string "1-")
  _ <- optional (char '(')
  npa <- count 3 digit
  _ <- optional (char ')')
  _ <- optional (oneOf " -")
  exc <- count 3 digit
  _ <- optional (oneOf " -")
  ln <- count 4 digit
  eof
  return $ PhoneNumber (read npa) (read exc) (read ln)
