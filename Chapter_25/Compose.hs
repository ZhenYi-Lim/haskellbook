{-# LANGUAGE InstanceSigs #-}

newtype Compose f g a =
    Compose { getCompose :: f (g a) }
    deriving (Show, Eq)

instance (Functor f, Functor g) => Functor (Compose f g) where
    fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
    pure :: a -> Compose f g a
    pure a = Compose (pure (pure a))
    (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
    (Compose fgab) <*> (Compose fga) = Compose ((pure (<*>)) <*> fgab <*> fga)

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
    foldMap fn (Compose fga)= (foldMap . foldMap) fn fga

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
    traverse f (Compose fga) = fmap Compose ((traverse . traverse) f fga)