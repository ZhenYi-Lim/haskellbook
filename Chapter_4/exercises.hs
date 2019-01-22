isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = x == (reverse x)

myAbs :: Integer -> Integer
myAbs x =
    if x < 0
        then -x
    else
        x

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f (a,b) (c,d) = ((b,d),(a,c))

func :: [Char] -> Int
func xs = w + 1
        where w = length xs

myId :: a -> a
myId x = x

head' :: (a,b) -> a
head' (a,b) = a