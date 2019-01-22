import Data.Char

notThe :: String -> String
notThe "the" = "a"
notThe xs = xs

replaceThe :: String -> String
replaceThe phrase = unwords $ go wordList []
    where
        wordList = words phrase
        go [] result = result
        go remain result = 
            go (tail remain) (result ++ [notThe $ head remain])

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel phrase = go wordList 0
            where
                wordList = words phrase
                go [] result = result
                go (x:xs) result =
                    if x == "the" && (elem (head (head xs)) "aeiou")
                        then go xs (result+1)
                    else go xs result

countVowels :: String -> Integer
countVowels str = go str 0
    where
        go "" result = result
        go (x:xs) result
            | elem (toLower x) "aeiou" = go xs (result+1)
            | otherwise = go xs result

newtype Word' = Word' String
    deriving (Eq, Show)

vowels = "aeiou"

mkWord :: String -> Maybe Word'
mkWord str = go str 0 0
        where
            go "" vo co
                | co > vo = Just (Word' str)
                | otherwise = Nothing
            go (x:xs) vo co
                | elem x vowels = go xs (vo+1) co
                | otherwise = go xs vo (co+1)

data Nat = Zero | Succ Nat
    deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ x) = (natToInteger x) + 1

integerToNat :: Integer -> Maybe Nat
integerToNat x =
    if x < 0
        then Nothing
    else Just (go x)
    where
        go 0 = Zero
        go x = Succ (go (x-1))

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust _ = True

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee y _ Nothing = y
mayybee _ f (Just x) = f x

fromMaybe :: a -> Maybe a -> a
fromMaybe y Nothing = y
fromMaybe y (Just x) = x

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:_) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

catMaybes :: [Maybe a] -> [a]
catMaybes list = go list []
        where
            go [] result = result
            go (Nothing:xs) result = go xs result
            go ((Just w):xs) result = go xs (result ++ [w])

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe list = go list []
            where
                go [] result = Just result
                go (Nothing:xs) result = Nothing
                go ((Just w):xs) result = go xs (result ++ [w])

lefts' :: [Either a b] -> [a]
lefts' list = foldr f [] list
                where
                    f (Left x) z = z ++ [x]
                    f _ z = z

rights' :: [Either a b] -> [b]
rights' list = foldr f [] list
                where
                    f (Right x) z = z ++ [x]
                    f _ z = z

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' list = ( (lefts' list), (rights' list) )

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _) = Nothing
eitherMaybe' f (Right x) = Just (f x)

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f1 _ (Left x) = f1 x
either' _ f2 (Right x) = f2 x

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' _ (Left _) = Nothing
eitherMaybe'' f (Right x) = Just $ either' id f (Right x)

myIterate :: (a -> a) -> a -> [a]
myIterate f start = start:(iterate f (f start))

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f start = x:(myUnfoldr f y)
                    where (Just (x,y)) = f start

betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = myUnfoldr f' x
                    where f' x' = Just (x',f x')

data BinaryTree a =
    Leaf
    | Node (BinaryTree a) a (BinaryTree a)
    deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfold f start = 
    case f start of
        Just (x,y,z) -> ( Node (unfold f x) y (unfold f z) )
        Nothing -> Leaf

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold f 0
    where f x
            | x == n = Nothing
            | otherwise = Just (x+1,x,x+1)