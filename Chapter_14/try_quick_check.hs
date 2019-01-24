import Test.QuickCheck
import Data.List (sort)
import Data.Char

half :: Fractional a => a -> a
half x = x / 2
 -- for a function half x = x / 2
-- this property should hold
halfIdentity :: Fractional a => a -> a
halfIdentity = (*2) . half

prop_halfIdentity :: Double -> Bool
prop_halfIdentity x = x == (halfIdentity x)

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs = snd $ foldr go (Nothing, True) xs
    where
        go _ status@(_, False) = status
        go y (Nothing, t) = (Just y, t)
        go y (Just x, _) = (Just y, x >= y)

listGen :: (Arbitrary a, Ord a) => Gen [a]
listGen = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    e <- arbitrary
    return [a,b,c,d,e]

prop_listOrdered :: (Arbitrary a, Ord a, Show a) => Gen [a] -> Property
prop_listOrdered listGenX = 
    forAll listGenX (\x -> listOrdered $ sort (x) )

plusAssociative :: (Num a, Eq a) => a -> a -> a -> Bool
plusAssociative x y z = x + (y + z) == (x + y) + z

plusCommutative :: (Num a, Eq a) => a -> a -> Bool
plusCommutative x y = x + y == y + x

uncurry2 :: (a -> b -> c) -> (a, b) -> c
uncurry2 f (x, y) = f x y

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) = f x y z

twoGen :: (Arbitrary a, Num a) => Gen (a, a)
twoGen = do
    a <- arbitrary
    b <- arbitrary
    return (a, b)

threeGen :: (Arbitrary a, Num a) => Gen (a, a, a)
threeGen = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (a, b, c)

prop_plusAssociative :: (Arbitrary a, Num a, Eq a, Show a) => Gen (a, a, a) -> Property
prop_plusAssociative threeGenX =
    forAll threeGenX (\x -> uncurry3 plusAssociative x)

prop_plusCommutative :: (Arbitrary a, Num a, Eq a, Show a) => Gen (a, a) -> Property
prop_plusCommutative twoGenX =
    forAll twoGenX (\x -> uncurry2 plusAssociative x)

multAssociative :: (Num a, Eq a) => a -> a -> a -> Bool
multAssociative x y z = x * (y * z) == (x * y) * z

multCommutative :: (Num a, Eq a) => a -> a -> Bool
multCommutative x y = x * y == y * x

prop_multAssociative :: (Arbitrary a, Num a, Eq a, Show a) => Gen (a, a, a) -> Property
prop_multAssociative threeGenX =
    forAll threeGenX (\x -> uncurry3 multAssociative x)

prop_multCommutative :: (Arbitrary a, Num a, Eq a, Show a) => Gen (a, a) -> Property
prop_multCommutative twoGenX =
    forAll twoGenX (\x -> uncurry2 multCommutative x)

checkQuotRem :: (Integral a, Eq a) => a -> a -> Bool
checkQuotRem x y = (quot x y)*y + (rem x y) == x

checkDivMod :: (Integral a, Eq a) => a -> a -> Bool
checkDivMod x y = (div x y)*y + (mod x y) == x

prop_checkQuotRem :: (Arbitrary a, Integral a, Eq a, Show a) => Gen (a, a) -> Property
prop_checkQuotRem twoGenX =
    forAll twoGenX (\x -> uncurry2 checkQuotRem x)

prop_checkDivMod :: (Arbitrary a, Integral a, Eq a, Show a) => Gen (a, a) -> Property
prop_checkDivMod twoGenX =
    forAll twoGenX (\x -> uncurry2 checkDivMod x)

twoGenNotDiv0 :: (Arbitrary a, Num a, Eq a) => Gen (a, a)
twoGenNotDiv0 = do
    a <- arbitrary
    b <- arbitrary `suchThat` (/=0)
    return (a, b)

expAssociative :: (Integral a, Eq a) => a -> a -> a -> Bool
expAssociative x y z = x ^ (y ^ z) == (x ^ y) ^ z

expCommutative :: (Integral a, Eq a) => a -> a -> Bool
expCommutative x y = x ^ y == y ^ x

twoGenGT0 :: (Arbitrary a, Num a, Ord a) => Gen (a, a)
twoGenGT0 = do
    a <- arbitrary `suchThat` (>0)
    b <- arbitrary `suchThat` (>0)
    return (a, b)

threeGenGT0 :: (Arbitrary a, Num a, Ord a) => Gen (a, a, a)
threeGenGT0 = do
    a <- arbitrary `suchThat` (>0)
    b <- arbitrary
    c <- arbitrary
    return (a, b, c)

prop_expAssociative :: (Arbitrary a, Integral a, Eq a, Show a) => Gen (a, a, a) -> Property
prop_expAssociative threeGenX =
    forAll threeGenX (\x -> uncurry3 expAssociative x)

prop_expCommutative :: (Arbitrary a, Integral a, Eq a, Show a) => Gen (a, a) -> Property
prop_expCommutative twoGenX =
    forAll twoGenX (\x -> uncurry2 expCommutative x)

checkReverseTwice :: Eq a => [a] -> Bool
checkReverseTwice xs = reverse (reverse xs) == id xs

prop_checkReverseTwice :: (Arbitrary a, Eq a, Show a) => Gen [a] -> Property
prop_checkReverseTwice listGenX = 
    forAll listGenX (\x -> checkReverseTwice x )

prop_fDollar :: (Eq b, Show a) => (a -> b) -> Gen a -> Property
prop_fDollar f gen =
    forAll gen (\x -> (f $ x) == f x)

prop_fComp :: (Show a, Eq c) => (b -> c) -> (a -> b) -> Gen a -> Property
prop_fComp f g gen =
    forAll gen (\x -> (f . g) x == f (g x))

oneGen :: (Arbitrary a, Num a) => Gen a
oneGen = do
    a <- arbitrary
    return a

prop_foldConcat :: (Show a, Eq a) => Gen [a] -> Property
prop_foldConcat listGenX =
    forAll listGenX (\x -> foldr (:) x x == (++) x x)

listOfListGen :: Arbitrary a => Gen [[a]]
listOfListGen = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    e <- arbitrary
    f <- arbitrary
    return [[a,b],[c,d],[e,f]]

prop_foldConcatEmpty :: (Show a, Eq a) => Gen [[a]] -> Property
prop_foldConcatEmpty listOfListGenX =
    forAll listOfListGenX (\x -> foldr (++) [] x == concat x)

lengthTakeN :: Int -> [a] -> Bool
lengthTakeN n xs = length (take n xs) == n

prop_lengthTakeN :: Property
prop_lengthTakeN =
    forAll (listOf (arbitrary :: Gen Int)) (\xs ->
        forAll (choose (0, length xs)) (\n -> lengthTakeN n xs))

checkReadShow :: (Read a, Show a, Eq a) => a -> Bool
checkReadShow x = (read (show x)) == x

prop_checkReadShow :: (Read a, Show a, Eq a) => Gen a -> Property
prop_checkReadShow genX =
    forAll genX (\x -> checkReadShow x)

square :: Float -> Float
square x = x * x

squareIdentity :: Float -> Float
squareIdentity = square . sqrt

posFloatGen :: Gen Float
posFloatGen = do
    a <- arbitrary `suchThat` (>0)
    return a

prop_squareIdentity ::  Gen Float -> Property
prop_squareIdentity posFloatGenX =
    forAll posFloatGenX
        (\x -> (squareIdentity x) == x)

twice :: (a -> a) -> (a -> a)
twice f = f . f

fourTimes :: (a -> a) -> (a -> a)
fourTimes = twice . twice

f' :: String -> Bool
f' x = (capitaliseWord x == twice capitaliseWord x) &&
        (capitaliseWord x == fourTimes capitaliseWord x)

f'' :: Ord a => [a] -> Bool
f'' x = (sort x == twice sort x) &&
        (sort x == fourTimes sort x)

capitaliseWord :: String -> String
capitaliseWord [] = []
capitaliseWord (x:xs) = (toUpper x) : xs

prop_checkf' :: Gen String -> Property
prop_checkf' stringGen =
    forAll stringGen (\x -> f' x)

prop_checkf'' :: (Ord a, Show a) => Gen [a] -> Property
prop_checkf'' listGenX =
    forAll listGenX (\x -> f'' x)

data Fool = Fulse | Frue
    deriving (Eq, Show)

foolGen :: Gen Fool
foolGen = oneof [return $ Fulse, return Frue]

instance Arbitrary Fool where
    arbitrary = foolGen

foolGen' :: Gen Fool
foolGen' = frequency [(2, return Fulse), (1, return Frue)]

runQC :: IO ()
runQC = do
    quickCheck prop_halfIdentity
    quickCheck $ prop_listOrdered (listGen :: Gen [Int])
    quickCheck $ prop_plusAssociative (threeGen :: Gen (Int, Int, Int))
    quickCheck $ prop_plusCommutative (twoGen :: Gen (Int, Int))
    quickCheck $ prop_multAssociative (threeGen :: Gen (Int, Int, Int))
    quickCheck $ prop_multCommutative (twoGen :: Gen (Int, Int))
    quickCheck $ prop_checkQuotRem (twoGenNotDiv0 :: Gen (Int, Int))
    quickCheck $ prop_checkDivMod (twoGenNotDiv0 :: Gen (Int, Int))
    quickCheck $ prop_expAssociative (threeGenGT0 :: Gen (Int, Int, Int))
    quickCheck $ prop_expCommutative (twoGenGT0 :: Gen (Int, Int))
    quickCheck $ prop_checkReverseTwice (listGen :: Gen [Int])
    quickCheck $ prop_fDollar (+1) (oneGen :: Gen Int)
    quickCheck $ prop_fComp (+1) (*2) (oneGen :: Gen Int)
    quickCheck $ prop_foldConcat (listGen :: Gen [Int])
    quickCheck $ prop_foldConcatEmpty (listOfListGen :: Gen [[Int]])
    quickCheck $ prop_lengthTakeN
    quickCheck $ prop_checkReadShow (listOf (arbitrary :: Gen Int))
    quickCheck $ prop_squareIdentity posFloatGen
    quickCheck $ prop_checkf' (listOf (arbitrary :: Gen Char))
    quickCheck $ prop_checkf'' (listOf (arbitrary :: Gen Char))