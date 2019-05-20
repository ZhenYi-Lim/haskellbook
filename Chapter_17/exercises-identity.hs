newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
fmap _ x = x

instance Applicative Identity where
pure = Identity
_ <*> x = x
