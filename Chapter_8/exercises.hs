cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

-- fill in the types
flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy "haha"

--"woops mrow woohoo!"
--"1 mrow haha"
--"woops mrow 2 mrow haha"
--"woops mrow blue mrow haha"
--"pink mrow haha mrow green mrow woops mrow blue"
--"are mrow Pugs mrow awesome"

sumtorial :: (Eq a, Num a) => a -> a
sumtorial 1 = 1
sumtorial n = n + sumtorial (n-1)

multBySum :: (Integral a) => a -> a -> a
multBySum num 0 = 0
multBySum num mul = num + multBySum num (mul-1)

data DividedResult = Result Integer | DividedByZero
    deriving Show

dividedBy :: Integer -> Integer -> (DividedResult, Integer)
dividedBy num denom = go num denom 0
    where go n d count
            | d == 0 = (DividedByZero, 0)
            | n < 0 = go (negate n) d count
            | d < 0 = go n (negate d) count
            | n < d = (Result (count 
                                * (if num < 0 then (-1) else 1)
                                * (if denom < 0 then (-1) else 1))
                                , n)
            | otherwise = go (n - d) d (count + 1)

mc91 :: (Num a, Ord a) => a -> a
mc91 n
    | n > 100 = n-10
    | otherwise = mc91(mc91(n+11))