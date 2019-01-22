stops = "pbtdkg"
vowels = "aeiou"

svs :: [(Char, Char, Char)]
svs = [(s1,v,s2) | s1 <- stops, v <- vowels, s2 <- stops]

svs' :: [(Char, Char, Char)]
svs' = filter (\(a,b,c) -> a == 'p') svs

nouns = ["apple", "ball", "car"]
verbs = ["digs", "eats", "fights"]

nvn :: [(String, String, String)]
nvn = [(n1,v,n2) | n1 <- nouns, v <- verbs, n2 <- nouns]

seekritFunc :: Fractional a => String -> a
seekritFunc x = (/) (fromIntegral(sum (map length (words x))))
                    (fromIntegral(length (words x)))

myAnd :: [Bool] -> Bool
myAnd = foldr (&&) True

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\x y -> (f x) || y)  False

myElem :: Eq a => a -> [a] -> Bool
myElem x = foldr (\a b -> (a == x) || b) False

myElem' :: Eq a => a -> [a] -> Bool
myElem' z = any (z == )

myReverse :: [a] -> [a]
myReverse = foldr (\x y -> y ++ [x]) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\x y -> f x : y ) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\x y -> if f x then x:y else y) []

squish :: [[a]] -> [a]
squish = foldr (\x y -> x ++ y) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr (\x y -> f x ++ y) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumByR :: (a -> a -> Ordering) -> [a] -> a
myMaximumByR f (x:xs) = foldr (\a b -> if f a b == GT then a else b) x xs

myMaximumByL :: (a -> a -> Ordering) -> [a] -> a
myMaximumByL f (x:xs) = foldl (\a b -> if f a b == GT then a else b) x xs

myMinimumByR :: (a -> a -> Ordering) -> [a] -> a
myMinimumByR f (x:xs) = foldr (\a b -> if f a b == LT then a else b) x xs

myMinimumByL :: (a -> a -> Ordering) -> [a] -> a
myMinimumByL f (x:xs) = foldl (\a b -> if f a b == LT then a else b) x xs
