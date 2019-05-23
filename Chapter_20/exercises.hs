data Constant a b = Constant b

instance Foldable (Constant a) where
    foldr f res (Constant x) = f x res

data Two a b = Two a b

instance Foldable (Two a) where
    foldr f res (Two _ y) = f y res

data Three a b c = Three a b c

instance Foldable (Three a b) where
    foldr f res (Three _ _ z) = f z res

data Three' a b = Three' a b b

instance Foldable (Three' a) where
    foldr f res (Three' _ y z) = f z (f y res)

data Four' a b = Four' a b b b

instance Foldable (Four' a) where
    foldr f res (Four' _ x y z) = f z (f y (f x res))

filterF :: (Applicative f, Foldable t, Monoid (f a))
        => (a -> Bool) -> t a -> f a
filterF f xs = foldr fn mempty xs
    where fn x res
            | f x = (pure x) <> res
            | otherwise = res