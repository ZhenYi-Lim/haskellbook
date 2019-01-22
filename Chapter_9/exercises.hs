import Data.Char

takeUpper :: String -> String
takeUpper x = filter isUpper x

capFirst :: String -> String
capFirst (x:xs) = (toUpper x) : xs

capAll :: String -> String
capAll [] = []
capAll (x:xs) = (toUpper x) : (capAll xs)

capHead :: String -> Char
capHead [] = ' '
capHead xs = toUpper (head xs)

myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x:xs) =
    if x == False
        then False
    else myAnd xs

myAnd' :: [Bool] -> Bool
myAnd' [] = True
myAnd' (x:xs) = x && myAnd' xs

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) = (f x) || (myAny f xs)

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem t (x:xs) = (x == t) || (myElem t xs)

myElem' :: Eq a => a -> [a] -> Bool
myElem' _ [] = False
myElem' t xs = any (\x-> t == x) xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]

squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ (squish xs)

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x:xs) = (f x) ++ (squishMap f xs)

squishAgain :: [[a]] -> [a]
squishAgain = squishMap (\x -> x)

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x:xs) = go f xs x
        where
            go _ [] curMax = curMax
            go f (x:xs) curMax =
                if (f x curMax) == GT
                    then go f xs x
                else go f xs curMax
            
myMaximumBy' :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy' f (x:y:[]) = if (f x y) == GT then x else y
myMaximumBy' f (x:y:xs) =
    if (f x y) == GT
        then myMaximumBy' f (x:xs)
    else myMaximumBy' f (y:xs)

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x:y:[]) = if (f x y) == LT then x else y
myMinimumBy f (x:y:xs) =
    if (f x y) == LT
        then myMinimumBy f (x:xs)
    else myMinimumBy f (y:xs)

myMaximum :: (Ord a) => [a] -> a
myMaximum xs = myMaximumBy compare xs

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare xs