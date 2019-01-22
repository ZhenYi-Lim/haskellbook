import Data.List (sort)

data Person = Person Bool
    deriving Show

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

data Mood = Blah | Woot
    deriving (Eq, Show)

settleDown :: Mood -> Mood
settleDown x =
    if x == Woot
        then Blah
    else x

type Subject = String
type Verb = String
type Object = String
data Sentence =
    Sentence Subject Verb Object
    deriving (Eq, Show)

--s1 = Sentence "dogs" "drool"
--s2 = Sentence "Julie" "loves" "dogs"

data Rocks =
    Rocks String
    deriving (Eq, Show, Ord)
data Yeah =
    Yeah Bool
    deriving (Eq, Show, Ord)
data Papu =
    Papu Rocks Yeah
    deriving (Eq, Show, Ord)

--phew = Papu (Rocks "chases") (Yeah True)
--truth = Papu (Rocks "chomskydoz") (Yeah True)

equalityForall :: Papu -> Papu -> Bool
equalityForall p p' = p == p'

comparePapus :: Papu -> Papu -> Bool
comparePapus p p' = p > p'

i :: Num a => a
i = 1

f :: Float
f = 1.0

f' :: RealFrac a => a
f' = 1.0

freud :: Ord a => a -> a
freud x = x

freud' :: Int -> Int
freud' x = x

--myX = 1 :: Int
--sigmund :: Int -> Int
--sigmund x = myX

--myX = 1 :: Int
--sigmund' :: Int -> Int
--sigmund' x = myX

jung :: Ord a => [a] -> a
jung xs = head (sort xs)

young :: [Char] -> Char
young xs = head (sort xs)

mySort :: [Char] -> [Char]
mySort = sort

signifier :: [Char] -> Char
signifier xs = head (mySort xs)

chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f1 x y = (f1 x) == y

arith :: Num b => (a -> b) -> Integer -> a -> b
arith f2 x y = (f2 y) + fromInteger(x)