module ChapterExercises where

import Data.Char
import Data.Bool

newtype Word' = Word' String deriving (Eq, Show)
vowels = "aeiou"

notThe :: String -> Maybe String
notThe str
  | str == "the" || str == "The" = Nothing
  | otherwise = Just str

mapOverWords :: (String -> String) -> String -> String
mapOverWords f = unwords . map f . words

replaceThe :: String -> String
replaceThe = mapOverWords replaceWithA
  where replaceWithA word = maybe "a" id (notThe word)

getIfVowel :: Char -> Maybe Char
getIfVowel c
  | (flip elem) vowels $ toLower c = Just c
  | otherwise = Nothing

isVowel :: Char -> Bool
isVowel = isJust . getIfVowel

startsWithVowel :: String -> Bool
startsWithVowel = isVowel . head

countTheBeforeVowel' :: [String] -> Integer
countTheBeforeVowel' [] = 0
countTheBeforeVowel' (w:[]) = 0
countTheBeforeVowel' (w1:w2:ws)
  | isJust $ notThe w1 = countTheBeforeVowel' (w2:ws)
  | otherwise = countTheBeforeVowel' ws + bool 0 1 (startsWithVowel w2)

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = countTheBeforeVowel' . words

countVowels :: String -> Integer
countVowels [] = 0
countVowels (c:cs) = countVowels cs + bool 1 0 (isVowel c)

mkWord :: String -> Maybe Word'
mkWord str = bool Nothing (Just (Word' str)) (vowelCount > consonantCount)
  where consonantCount = wordLength - vowelCount
        wordLength = toInteger $ length str
        vowelCount = countVowels str


data Nat = Zero | Succ Nat deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ n) = 1 + natToInteger n

integerToNat :: Integer -> Maybe Nat
integerToNat n
  | n < 0 = Nothing
  | otherwise = Just $ toNat n
      where toNat n
              | n == 0 = Zero
              | otherwise = Succ . toNat $ n - 1

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust _       = True

isNothing :: Maybe a -> Bool
isNothing = not . isJust

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee x _ Nothing  = x
mayybee _ f (Just x) = f x

fromMaybe :: a -> Maybe a -> a
fromMaybe x = maybe x id

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:_) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

catMaybes :: [Maybe a] -> [a]
catMaybes = map extract . filter isJust
  where extract (Just x) = x

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe = go []
  where go acc [] = Just $ reverse acc
        go acc (x:xs) = case x of
          Just x' -> go (x':acc) xs
          Nothing -> Nothing

lefts' :: [Either a b] -> [a]
lefts' = foldr extractLefts []
  where extractLefts (Left x) acc = x:acc
        extractLefts _        acc = acc

rights' :: [Either a b] -> [b]
rights' = foldr extractRights []
  where extractRights (Right x) acc = x:acc
        extractRights _         acc = acc

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' eithers = (lefts' eithers, rights' eithers)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _) = Nothing
eitherMaybe' f (Right x) = Just $ f x

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left x) = f x
either' _ f (Right x) = f x

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (\_ -> Nothing) (\x -> Just $ f x)

iterate' :: (a -> a) -> a -> [a]
iterate' f = unfoldr' (\x -> Just (x, f x))

unfoldr' :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr' f a = case (f a) of
  Just (x, y) -> x : (unfoldr' f y)
  Nothing -> []

data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

insert :: Ord a => a -> BinaryTree a -> BinaryTree a
insert b Leaf = Node Leaf b Leaf
insert b (Node left a right)
  | b < a     = Node (insert b left) a right
  | b > a     = Node left a (insert b right)
  | otherwise = (Node left a right)

mapTree :: (a -> b)
        -> BinaryTree a
        -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) = inorder left ++ [a] ++ inorder right

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) = [a] ++ preorder left ++ preorder right

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left a right) = postorder left ++ postorder right ++ [a]

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree _ acc Leaf = acc
foldTree f acc (Node left a right) = f a (foldTree f (foldTree f acc right) left)

foldTree' :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree' f acc = foldr f acc . inorder

unfoldTree :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfoldTree f a = case (f a) of
  Nothing -> Leaf
  Just (left, value, right) -> Node (unfoldTree f left) value (unfoldTree f right)

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfoldTree (\x -> if x >= n then Nothing else Just (x+1, x, x+1)) 0
