data FixMePls a =
    FixMe
    | Pls a
    deriving (Eq, Show)
    
instance Functor FixMePls where
    fmap _ FixMe = FixMe
    fmap f (Pls a) = Pls (f a)

data WhoCares a =
    ItDoesnt
    | Matter a
    | WhatThisIsCalled
    deriving (Eq, Show)

instance Functor WhoCares where
    fmap _ ItDoesnt = ItDoesnt
    fmap _ WhatThisIsCalled = WhatThisIsCalled
    fmap f (Matter a) = Matter (f a)

data CountingGood a =
    Heisenberg Int a
    deriving (Eq, Show)

instance Functor CountingGood where
    fmap f (Heisenberg n a) = Heisenberg (n) (f a)

-- :t (.)
-- (b -> c) -> (a -> b) -> a -> c

-- fmap :: Functor f => (m -> n) -> f m -> f n
-- fmap :: Functor g => (x -> y) -> g x -> g y

-- :t . fmap
-- (a1 -> a2 -> b) -> a1 -> f a2 -> f b

-- :t fmap . fmap
-- (a -> b) -> f1 (f2 a) -> f1 (f2 b)

-- b is (m -> n)
-- c is f m -> f n

-- :t (.) fmap
-- ((m -> n) -> (f m -> f n)) -> (a -> (m -> n)) -> a -> (f m -> f n)
-- (a -> m -> n) -> a -> (f m -> f n)

-- x is (a -> m -> n)
-- y is a -> (f m -> f n)

-- :t fmap (.) fmap
-- ((a -> m -> n) -> a -> (f m -> f n)) -> (g (a -> m -> n)) -> (g (a -> f m -> f n))
-- (g (a -> m -> n)) -> (g (a -> f m -> f n)) ???

-----------------------------------------------------

-- b is (m -> n)
-- c is f m -> f n
-- a is (x -> y)
-- b is g x -> g y

-- m is g x
-- n is g y

-- c is f g x -> f g y

-- :t fmap . fmap
-- ((m -> n) -> f m -> f n) -> ((x -> y) -> (m -> n)) -> (x -> y) -> f g x -> f g y 
-- (x -> y) -> f g x -> f g y 

data Two a b = Two a b deriving (Eq, Show)

data Or a b = First a | Second b deriving (Eq, Show)

instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)

instance Functor (Or a) where
    fmap f (First a) = First a
    fmap f (Second b) = Second (f b)

data Possibly a = LolNope | Yeppers a deriving (Eq, Show)
    
instance Functor Possibly where
    fmap f (Yeppers a) = Yeppers (f a)
    fmap f LolNope = LolNope

incIfRight :: Num a => Either e a -> Either e a
incIfRight (Right n) = Right $ n + 1
incIfRight (Left e) = Left e
    
showIfRight :: Show a => Either e a -> Either e String
showIfRight (Right s) = Right $ show s
showIfRight (Left e) = Left e

incEither :: Num a => Either e a -> Either e a
incEither m = fmap (+1) m

showEither :: Show a => Either e a -> Either e String
showEither s = fmap show s

incEither' :: Num a => Either e a -> Either e a
incEither' = fmap (+1)

showEither' :: Show a => Either e a -> Either e String
showEither' = fmap show

liftedInc :: (Functor f, Num b) => f b -> f b
liftedInc = fmap (+1)

liftedShow :: (Functor f, Show a) => f a -> f String
liftedShow = fmap show

data Sum a b = First' a | Second' b deriving (Eq, Show)

instance Functor (Sum a) where
    fmap f (First' a) = First' a
    fmap f (Second' b) = Second' (f b)

data Wrap f a = Wrap (f a) deriving (Eq, Show)

instance Functor f => Functor (Wrap f) where
    fmap f (Wrap fa) = Wrap (fmap f fa)

type Nat f g = forall a . f a -> g a

-- This'll work
maybeToList :: Nat Maybe []
maybeToList Nothing = []
maybeToList (Just a) = [a]

-- This will not work, not allowed.
{- degenerateMtl :: Nat Maybe []
degenerateMtl Nothing = []
degenerateMtl (Just a) = [a+1] -}