import Data.Char

cap :: [Char] -> [Char]
cap = map toUpper

rev :: [Char] -> [Char]
rev = reverse

composed :: [Char] -> [Char]
composed = cap . rev

fmapped :: [Char] -> [Char]
fmapped = fmap cap rev

tupled :: [Char] -> ([Char], [Char])
tupled = (,) <$> cap <*> rev

tupledDo :: [Char] -> ([Char], [Char])
tupledDo = do
  r <- rev
  c <- cap
  return $ (c, r)

tupledBind :: [Char] -> ([Char], [Char])
tupledBind =
  rev >>=
    \r -> cap >>=
      \c -> return (c, r)
