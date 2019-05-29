{-# LANGUAGE InstanceSigs #-}

newtype Moi s a = Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
    fmap :: (a -> b) -> Moi s a -> Moi s b
    fmap f (Moi g) = Moi $ \s -> let (a, s') = g s in (f a, s')

instance Applicative (Moi s) where
    pure :: a -> Moi s a
    pure a = Moi $ \s -> (a, s)

    (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
    (Moi f) <*> (Moi g) = Moi $ \s -> let (func, sf) = f s
                                          (a, sa) = g sf
                                      in (func a, sa)

instance Monad (Moi s) where
    return = pure

    (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
    -- Why does this not work?
    --(Moi f) >>= g = g . fst . f
    (Moi f) >>= g = Moi $ \s -> let (a, sa) = f s
                                in runMoi (g a) $ sa