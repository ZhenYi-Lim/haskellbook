tensDigit :: Integral a => a -> a
tensDigit x = d
    where   (xLast, _) = x `divMod` 10
            (_, d) = xLast `divMod` 10

hundredsDigit :: Integral a => a -> a
hundredsDigit x = d
    where   (xLast, _) = x `divMod` 100
            (_, d) = xLast `divMod` 10
            
foldBool :: a -> a -> Bool -> a
foldBool x _ False = x
foldBool _ y True = y

foldBool' :: a -> a -> Bool -> a
foldBool' x y bool =
    case bool of
        False -> x
        True -> y

foldBool'' :: a -> a -> Bool -> a
foldBool'' x y bool
    | bool = y
    | otherwise = x

g :: (a -> b) -> (a, c) -> (b, c)
g f (x, y) = (f x, y)

roundTrip :: (Show a, Read a) => a -> a
roundTrip = read . show