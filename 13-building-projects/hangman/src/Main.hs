module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.IO (hSetBuffering, stdout, BufferMode (NoBuffering))
import System.Random (randomRIO)

newtype WordList = WordList [String] deriving (Eq, Show)

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return $ WordList (lines dict)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

gameWords :: IO WordList
gameWords = do
  (WordList aw) <- allWords
  return $ WordList (filter gameLength aw)
    where gameLength w =
            let l = length (w :: String)
             in l >= minWordLength && l <  maxWordLength

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
  randomIndex <- randomRIO (0, length wl)
  return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

data Puzzle =
  Puzzle String [Maybe Char] [Char]

instance Show Puzzle where
  show (Puzzle _ discovered guessed) =
    (intersperse ' ' $
     fmap renderPuzzleChar discovered)
    ++ " Guessed so far: " ++ guessed

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar (Just c) =  c
renderPuzzleChar Nothing  = '_'

freshPuzzle :: String -> Puzzle
freshPuzzle word = Puzzle word (map (const Nothing) word) []

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle word _ _) = (flip elem) word

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessed) = (flip elem) guessed

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word discovered guessed) c =
  Puzzle word newDiscovered (c : guessed)
    where newDiscovered = zipWith (zipper c) word discovered
          zipper guess wordChar guessedChar =
            if wordChar == guess
              then Just wordChar
              else guessedChar

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "You guessed: " ++ [guess]
  case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
    (_, True) -> do
      putStrLn "You already guessed that character \
               \pick something else!"
      return puzzle
    (True, _) -> do
      putStrLn "You guessed correctly!"
      return (fillInCharacter puzzle guess)
    (False, _) -> do
      putStrLn "You guessed incorrectly"
      return (fillInCharacter puzzle guess)

incorrectGuesses :: Puzzle -> String
incorrectGuesses (Puzzle word _ guessed) = filter (not . (flip elem) word) guessed

gameOver :: Puzzle -> IO ()
gameOver puzzle@(Puzzle word _ guessed) =
  if (length $ incorrectGuesses puzzle) > 7
    then do
      putStrLn "You lose!"
      putStrLn $ "The word was " ++ word
      exitSuccess
    else return ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle word discovered _) =
  if all isJust discovered
    then do
      putStrLn "You win!"
      putStrLn $ "The word was: " ++ word
      exitSuccess
    else return()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameOver puzzle
  gameWin puzzle
  putStrLn $ "Current puzzle is: " ++ show puzzle
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _   -> putStrLn "You must only guess a single letter"

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  word <- randomWord'
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle
