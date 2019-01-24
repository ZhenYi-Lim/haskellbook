module Addition where

import Test.Hspec
import Test.QuickCheck

sayHello :: IO ()
sayHello = putStrLn "hello!"

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
    where go n d count
            | n < d = (count, n)
            | otherwise = go (n - d) d (count + 1)

multipliedBy :: (Eq a, Num a) => a -> a -> a
multipliedBy num mult = go num mult 0
    where go n m result
            | m == 0 = result
            | otherwise = go n (m-1) (result+n)

trivialInt :: Gen Int
trivialInt = return 1

oneThroughThree :: Gen Int
oneThroughThree = elements [1, 2, 3]

oneThroughThree' :: Gen Int
oneThroughThree' = elements [1, 2, 2, 2, 2, 3]

genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [False, True]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char
genChar = elements ['a'..'z']

genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = do
    a <- arbitrary
    b <- arbitrary
    return (a, b)

genThreeple :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (a, b, c)
genThreeple = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (a, b, c)

genEither :: (Arbitrary a, Arbitrary b) => Gen (Either a b)
genEither = do
    a <- arbitrary
    b <- arbitrary
    elements [Left a, Right b]
    -- equal probability

genMaybe :: Arbitrary a => Gen (Maybe a)
genMaybe = do
    a <- arbitrary
    elements [Nothing, Just a]

-- What QuickCheck does so you get more Just values
genMaybe' :: Arbitrary a => Gen (Maybe a)
genMaybe' = do
    a <- arbitrary
    frequency [ (1, return Nothing) , (3, return (Just a))]
    -- frequency :: [(Int, Gen a)] -> Gen a

prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 1 > x

runQc :: IO ()
runQc = quickCheck prop_additionGreater

main :: IO ()
main = hspec $ do
    describe "Addition" $ do
        it "1 + 1 is greater than 1" $ do
            (1 + 1) > (1 :: Integer)  `shouldBe` True
        it "2 + 2 is equal to 4" $ do
            2 + (2 :: Integer) `shouldBe` 4
        it "15 divided by 3 is 5" $ do
            dividedBy 15 (3 :: Integer) `shouldBe` (5,0)
        it "22 divided by 5 is 4" $ do
            dividedBy 22 (5 :: Integer) `shouldBe` (4,2)
        it "2 multiplied by 3 is 6" $ do
            multipliedBy 2 (3 :: Integer) `shouldBe` 6
        it "x + 1 is always greater than x" $ do
            property $ \x -> x + 1 > (x :: Int)