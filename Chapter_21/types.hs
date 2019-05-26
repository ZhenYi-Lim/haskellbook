(.) :: (b -> c) -> (a -> b) -> a -> c

sequence :: (Traversable t, Monad m) => t (m a) -> m (t a)

fmap :: Functor f => (a -> b) -> f a -> f b

sequence . fmap :: (Traversable ((->) (m a1)), Monad m) => 
(a1 -> a2) -> m (m a1 -> a2)

(t (m a) -> m (t a)) -> (a1 -> t (m a)) -> a1 -> m (t a)
(t (m a) -> m (t a)) -> ((a2 -> b) -> f a2 -> t (m a)) -> (a2 -> b) -> f a2 -> m (t a)
(a2 -> b) -> f a2 -> m (t a)

b = t (m a1)
c = m (t a1)
a - (a2 -> b1) -> f a2
b = f b1
f = t
b1 = m a1

(a2 -> m a1) -> t a2 -> m (t a1) ???
