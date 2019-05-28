import Control.Applicative

boop = (*2)
doop = (+10)

bip :: Integer -> Integer
bip = boop . doop

bloop :: Integer -> Integer
bloop = fmap boop doop
--Functor of functions.
--fmap boop doop x == (*2) ((+10) x)
--                 == (*2) . (+10)
-- *The Functor of functions is function composition.

bbop :: Integer -> Integer
bbop = (+) <$> boop <*> doop

duwop :: Integer -> Integer
duwop = liftA2 (+) boop doop
-- liftA2 :: (a -> b -> c) -> f a -> f b -> f c
-- f Integer -> f Integer -> f Integer, f == (a ->)
-- (a -> Integer) -> (a -> Integer) -> (a -> Integer), a == Integer
-- Integer -> Integer
-- * The Applicative and Monad chain the argument forward in addition to the composition.

boopDoop :: Integer -> Integer
boopDoop = do
    a <- boop
    b <- doop
    return (a + b)

--  Reader is a way of stringing functions together when all those functions are awaiting one input from a shared environment.