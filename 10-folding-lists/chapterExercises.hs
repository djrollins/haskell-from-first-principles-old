-- Warmup & Review
-- 1
stops = "pbtdkg"
vowels = "aeiou"
-- a
tuples1 = [(s1,v,s2) | s1 <- stops, v <- vowels, s2 <- stops]
-- b
tuples2 = [(s1,v,s2) | s1 <- stops, v <- vowels, s2 <- stops, s1 == 'p']
-- c
nouns = ["burger", "cat", "scooter"]
verbs = ["eats", "pets", "rides"]

nvn = [(n1,v,n2) | n1 <- nouns, v <- verbs, n2 <- nouns]


-- 2
avgWordLength x = div (sum (map length (words x)))
                      (length (words x))
-- 3
avgWordLengthPrecise x = fromIntegral (sum (map length (words x))) / fromIntegral (length (words x))

-- Rewriting functions using folds
-- 1
myOr :: [Bool] -> Bool
myOr = foldr (||) False
-- 2
myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr ((||) . f) False
-- 3
myElem :: Eq a => a -> [a] -> Bool
myElem e = foldr ((||) . (==e)) False
-- 4
myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []
-- 5
myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:) . f) []
-- 6
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p = foldr f []
    where f a b
            | p a = a : b
            | otherwise = b
-- 7
squish :: [[a]] -> [a]
squish = foldr (++) []
-- 8
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr ((++) . f) []
-- 9
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id
-- 10
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [x] = x
myMaximumBy o (x:xs) = foldl f x xs
    where f a b
            | o a b == GT = a
            | otherwise = b
-- 11
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ [x] = x
myMinimumBy o (x:xs) = foldl f x xs
    where f a b
            | o a b == LT = a
            | otherwise = b
