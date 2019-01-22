eftBool :: Bool -> Bool -> [Bool]
eftBool True False = []
eftBool False False = [False]
eftBool True True = [True]
eftBool False True = [False, True]

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd x y
    | x > y = []
    | x == y = [x]
    | x < y = x : (eftOrd (succ x) y)

eftInt :: Int -> Int -> [Int]
eftInt x y
    | x > y = []
    | x == y = [x]
    | x < y = x : (eftInt (succ x) y)

eftChar :: Char -> Char -> [Char]
eftChar x y
    | x > y = []
    | x == y = [x]
    | x < y = x : (eftChar (succ x) y)