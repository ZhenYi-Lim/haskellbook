import Data.Monoid

sum' :: (Foldable t, Num a) => t a -> a
sum' xs = foldr (+) 0 xs

product' :: (Foldable t, Num a) => t a -> a
product' xs = getProduct(foldMap Product xs)

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' y xs = foldr (\n res -> (n == y) || res) False xs

minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' xs = foldr fn Nothing xs
    where fn x Nothing = Just x
    fn x (Just y)
            | x < y = Just x
            | otherwise = Just y

maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' xs = foldr fn Nothing xs
    where fn x Nothing = Just x
          fn x (Just y)
            | x > y = Just x
            | otherwise = Just y

null' :: (Foldable t) => t a -> Bool
null' xs = foldr (\_ _ -> False) True xs

length' :: (Foldable t) => t a -> Int
length' xs = foldr (\_ res -> res + 1) 0 xs

toList' :: (Foldable t) => t a -> [a]
toList' xs = foldr (:) [] xs

fold' :: (Foldable t, Monoid m) => t m -> m
fold' xs = foldMap id xs

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f xs = foldr (\x res -> (f x) <> res) mempty xs 