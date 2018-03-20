-- A playground to figure out Monads

module Scratch where

import Control.Monad

-- Monad is an Applicative Functor...? It is a generalization of concat.

-- Gets an Int value from IO
getInt :: IO Int
getInt = liftM read getLine

-- Gets n lines from IO
getLines :: Int -> IO [String]
getLines 0 = return $ []
getLines n = liftM2 (:) getLine (getLines (n - 1))

-- Gets an Int value from IO and then gets that many lines
getNLines :: IO [String]
getNLines = getInt >>= getLines

-- (>>=) :: Monad m => m a -> (a -> m b) -> m b
-- (>>=) :: IO Int -> (Int -> IO [String]) -> IO [String]

getNLinesDo :: IO [String]
getNLinesDo = do
  n <- getInt
  getLines n

getNLinesAreEqual :: IO Bool
-- getNLinesAreEqual = (==) <$> getNLinesDo <*> getNLines
getNLinesAreEqual = liftM2 (==) getNLinesDo getNLines

-- From REPL:
--    *Scratch> getNLinesAreEqual
--    1
--    hello
--    1
--    hello
--    True

sequencing :: IO ()
sequencing = do
  putStrLn "hello"
  putStrLn "world"

sequencing' :: IO ()
sequencing' = putStrLn "hello" >> putStrLn "world"

-- using *> from Applicative
sequencing'' :: IO ()
sequencing'' = putStrLn "hello" *> putStrLn "world"

-- the sequenceing functions are all equivalent.
-- `do` notation just puts `>>` between statements if they are separate and
-- `>>=` if a value if the `a` in `Monoid m => m a` is used on the rhs

binding :: IO ()
binding = do
  name <- getLine
  putStrLn name

binding' :: IO ()
binding' = getLine >>= putStrLn

binding'' :: IO ()
binding'' = join $ putStrLn <$> getLine

-- the binding functions above are equivalent. `>>=` is equivalent to `fmap`ping a
-- function that produces a Monad and `join`ing the resulting nested Monads.
--
-- [a] is an instance of Monad

plusOneAndPutInList :: Num a => a -> [a]
plusOneAndPutInList x = [x+1]

-- plusOneAndPutInList 2 == [2]

fmapPlusOneAndPutInList :: Num a => [a] -> [[a]]
fmapPlusOneAndPutInList = fmap plusOneAndPutInList

-- fmapPlusOneAndPutInList [2] == [[3]]

concatFmapPlusOneAndPutInList :: Num a => [a] -> [a]
concatFmapPlusOneAndPutInList = concat . fmapPlusOneAndPutInList

-- concatFmapPlusOneAndPutInList [2] == [3]

joinFmapPlusOneAndPutInList :: Num a => [a] -> [a]
joinFmapPlusOneAndPutInList = join . fmapPlusOneAndPutInList

-- joinFmapPlusOneAndPutInList [2] == [3]

bindPlusOneAndPutInList :: Num a => [a] -> [a]
bindPlusOneAndPutInList xs = xs >>= plusOneAndPutInList

-- bindPlusOneAndPutInList [2] == [3]

-- do syntax cleans up heavy nested binding:

twoBinds :: IO ()
twoBinds = do
  putStrLn "name pls:"
  name <- getLine
  putStrLn "age pls:"
  age <- getLine
  putStrLn ("y helo thar: "
            ++ name ++ " who is: "
            ++ age ++ " years old.")

twoBinds' :: IO ()
twoBinds' =
  putStrLn "name pls:" >>
    getLine >>=
      \name -> putStrLn "age pls:" >>
        getLine >>=
          \age -> putStrLn ("y helo thar: "
                            ++ name ++ " who is: "
                            ++ age ++ " years old.")


-- Kleilsi composition. Composing two functions that return Monads

sayHi :: String -> IO String
sayHi greeting = do
  putStrLn greeting
  getLine

readM :: Read a => String -> IO a
readM = return . read

getAge :: String -> IO Int
getAge = sayHi >=> readM

askForAge :: IO Int
askForAge = getAge "Hello! how old are you? "

-- Composing the greeter function and itself will echo forever
echo :: String -> IO String
echo = sayHi >=> echo
