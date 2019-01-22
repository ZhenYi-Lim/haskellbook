{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

data MyType = MyVal Int
    deriving (Eq, Show)

class TooMany a where
    tooMany :: a -> Bool

instance TooMany Int where
    tooMany n = n > 42

newtype Goats = Goats Int
    deriving (Eq, Show, TooMany)

--instance TooMany Goats where
--    tooMany (Goats n) = n > 43

instance TooMany (Int, String) where
    tooMany (n, _) = n > 42

instance TooMany (Int, Int) where
    tooMany (n, m) = (n+m) > 42

instance (Num a, TooMany a) => TooMany (a, a) where
    tooMany (n, m) = tooMany (n+m)