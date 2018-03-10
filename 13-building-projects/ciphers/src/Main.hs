module Main where

import Data.Char
import Data.List
import System.IO (hSetBuffering, stdout, BufferMode (NoBuffering))
import System.Environment (getArgs)
import Text.Read (readMaybe)

data Mode = Encode | Decode deriving (Eq, Show)
data Cipher = Vigenere String | Ceaser Int deriving (Eq, Show)
data Command = Command Cipher Mode String deriving (Eq, Show)
data Error = InvalidMode | InvalidCipher | InvalidParameter String deriving (Eq, Show)

type Validated a = Either [Error] a

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


shiftLetter :: Int -> Char -> Char
shiftLetter count char
  | isUpper char = shift' 'A'
  | isLower char = shift' 'a'
  | isSpace char = char
  | otherwise = error $ char : " is not an alphabetic character"
    where shift' base =
            chr $ (((ord char) - (ord base) + count) `mod` 26) + (ord base)

ceaser :: Int -> String -> String
ceaser count = map . shiftLetter $ count

unCeaser :: Int -> String -> String
unCeaser count = map . shiftLetter . negate $ count

mkVigenere :: String -> Validated Cipher
mkVigenere param
  | all isAlpha param = Right . Vigenere $ upCase param
  | otherwise = Left [InvalidParameter "key must only contain alphabetical characters"]
    where upCase (c:cs) = (toUpper c) : (upCase cs)
          upCase "" = ""

mkCeaser :: String -> Validated Cipher
mkCeaser param = case readMaybe param of
  Just count -> Right (Ceaser count)
  otherwise -> Left [InvalidParameter "expected a number"]

mkCipher :: String -> String -> Validated Cipher
mkCipher cipher param = case cipher of
  "vigenere" -> mkVigenere param
  "ceaser" -> mkCeaser param
  otherwise -> Left [InvalidCipher]

mkMode :: String -> Validated Mode
mkMode mode = case mode of
  "encode" -> Right Encode
  "decode" -> Right Decode
  otherwise -> Left [InvalidMode]

mkCommand :: Validated Mode -> Validated Cipher -> String -> Validated Command
mkCommand (Right mode) (Right cipher) input = Right (Command cipher mode input)
mkCommand (Left modeErr) (Left cipherErr) _ = Left (modeErr ++ cipherErr)
mkCommand (Left err) _ _ = Left err
mkCommand _ (Left err) _ = Left err

handleArgs :: [String] -> Validated Command
handleArgs (mode:cipher:arg:input:[]) =
  mkCommand (mkMode mode) (mkCipher cipher arg) input

translateError :: Error -> String
translateError InvalidMode = "invalid mode [encode|decode]"
translateError InvalidCipher = "invalid cipher [ceaser|vigenere]"
translateError (InvalidParameter err) = "invalid Parameter: " ++ err

runCommand :: Validated Command -> String
runCommand (Left errs) = concat . intersperse "\n" $ map translateError errs
runCommand (Right cmd) = dispatch cmd
  where dispatch (Command (Ceaser count) Encode input) = ceaser count input
        dispatch (Command (Ceaser count) Decode input) = unCeaser count input
        dispatch (Command (Vigenere keyword) Encode input) = vigenere keyword input
        dispatch (Command (Vigenere _) Decode _) = undefined

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  args <- getArgs
  if (length args /= 4)
    then error "Incorrect number of arguments"
    else putStrLn . runCommand $ handleArgs args
