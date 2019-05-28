{-# LANGUAGE InstanceSigs #-}

myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f x y = f <$> x <*> y

newtype Reader r a =
    Reader { runReader :: r -> a }

asks :: (r -> a) -> Reader r a
asks f = Reader f

instance Functor (Reader r) where
    --fmap :: (a -> b) -> (r -> a) -> (r -> b)
    fmap f (Reader ra) = Reader $ f . ra

instance Applicative (Reader r) where
    pure :: a -> Reader r a
    pure a = Reader $ \r -> a

    (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
    (Reader rab) <*> (Reader ra) = Reader $ \r -> rab r (ra r)

instance Monad (Reader r) where
    return :: a -> Reader r a
    return = pure
    
    (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
    (Reader ra) >>= aRb = Reader $ \r -> runReader (aRb (ra r)) r

newtype HumanName = HumanName String deriving (Eq, Show)

newtype DogName = DogName String deriving (Eq, Show)

newtype Address = Address String deriving (Eq, Show)

data Person =
    Person { humanName :: HumanName
            , dogName :: DogName
            , address :: Address
    } deriving (Eq, Show)

data Dog =
    Dog { dogsName :: DogName
        , dogsAddress :: Address
    } deriving (Eq, Show)

getDogRM' :: Reader Person Dog
getDogRM' = undefined
--no idea
--getDogRM' = Reader (Dog <$> dogName <*> address)