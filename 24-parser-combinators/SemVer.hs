module SemVer where

import Text.Trifecta
import Control.Applicative
import Data.Functor (void)
import Data.Monoid ((<>))

data NumberOrString =
    NOSS String
  | NOSI Integer
  deriving (Eq, Show)

type Major = Integer
type Minor = Integer
type Patch = Integer

type Release = [NumberOrString]
type Metadata = [NumberOrString]

semVerInt :: Parser Integer
semVerInt = do
  digits <- try $ some digit <* notFollowedBy letter
  if length digits > 1 && head digits == '0'
    then fail "SemVer integer cannot start with 0"
    else return $ read digits

semVerExtension :: Parser NumberOrString
semVerExtension = NOSI <$> semVerInt <|> NOSS <$> some alphaNum

semVerExtensionList :: Parser [NumberOrString]
semVerExtensionList = do
  init <- many (try (semVerExtension <* dot))
  last <- semVerExtension
  return (init ++ [last])

semVerRelease :: Parser Release
semVerRelease = char '-' *> semVerExtensionList <|> return []

semVerMetadata :: Parser Metadata
semVerMetadata = char '+' *> semVerExtensionList <|> return []

data SemVer =
  SemVer Major Minor Patch Release Metadata
  deriving (Show)

parseSemVer :: Parser SemVer
parseSemVer = SemVer <$>
  semVerInt <* dot <*>
  semVerInt <* dot <*>
  semVerInt <*>
  semVerRelease <*>
  semVerMetadata <* (void space <|> eof)

instance Eq SemVer where
  (SemVer maj min pat rel _) == (SemVer maj' min' pat' rel' _) =
    maj == maj' && min == min' && pat == pat' && rel == rel'

-- TODO: compare releases
instance Ord SemVer where
  (SemVer maj min pat rel _) `compare` (SemVer maj' min' pat' rel' _) =
    maj `compare` maj' <> min `compare` min' <> pat `compare` pat'
