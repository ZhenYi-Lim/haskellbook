{-# LANGUAGE NoMonomorphismRestriction #-}
module DetermineTheType where
-- simple example
example = 1

bigNum = (^) 5
wahoo = bigNum $ 10

x = print
y = print "woohoo!"
z = x "hello world"

a = (+)
b = 5
c = a 10
d = c 200

e = 12 + f
f = 10000 * g
g = 1

functionH :: [a] -> a
functionH (x:_) = x

functionC :: (Ord a) => a -> a -> Bool
functionC x y =
    if (x > y) 
        then True 
    else False

functionS :: (a,b) -> b
functionS (x, y) = y

i :: a -> a
i x = x

c0 :: a -> b -> a
c0 x y = x

c'' :: b -> a -> b
c'' x y = x

c' :: a -> b -> b
c' x y = y

r :: [a] -> [a]
r x = x

co :: (b -> c) -> (a -> b) -> a -> c
co f1 f2 x = f1 (f2 x)

a0 :: (a -> c) -> a -> a
a0 f1 x = x

a' :: (a -> b) -> a -> b
a' f1 x = f1 x

    