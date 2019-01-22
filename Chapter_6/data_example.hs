data Trivial = 
    Trivial'

instance Eq Trivial where
    Trivial' == Trivial' = True

data DayOfWeek =
    Mon | Tue | Wed | Thu | Fri | Sat | Sun
    deriving Show

data Date =
    Date DayOfWeek Int
    deriving Show

instance Eq DayOfWeek where
    (==) Mon Mon = True
    (==) Tue Tue = True
    (==) Wed Wed = True
    (==) Thu Thu = True
    (==) Fri Fri = True
    (==) Sat Sat = True
    (==) Sun Sun = True
    (==) _ _ = False

instance Eq Date where
    (==) (Date weekday dayOfMonth) (Date weekday' dayOfMonth') =
        weekday == weekday' && dayOfMonth == dayOfMonth'

data Identity a =
    Identity a

instance Eq a => Eq (Identity a) where
    (==) (Identity v) (Identity v') = v == v'

data NoEq =
    NoEqInst
    deriving Show

data TisAnInteger =
    TisAn Integer
    deriving Show

instance Eq TisAnInteger where
    (==) (TisAn x) (TisAn x') = x == x'

data TwoIntegers =
    Two Integer Integer
    deriving Show

instance Eq TwoIntegers where
    (==) (Two x y) (Two x' y') = x == x' && y == y'

data StringOrInt =
    TisAnInt Int | TisAString String
    deriving Show

instance Eq StringOrInt where
    (==) (TisAnInt x) (TisAnInt x') = x == x'
    (==) (TisAString x) (TisAString x') = x == x'
    (==) _ _ = False

data Pair a =
    Pair a a
    deriving Show

instance Eq a => Eq (Pair a) where
    (==) (Pair x y) (Pair x' y') = x == x' && y == y'

data Tuple a b =
    Tuple a b
    deriving Show

instance (Eq a, Eq b) => Eq (Tuple a b) where
    (==) (Tuple x y) (Tuple x' y') = x == x' && y == y'

data Which a =
    ThisOne a | ThatOne a
    deriving (Ord, Show)

instance (Eq a) => Eq (Which a) where
    (==) (ThisOne x) (ThisOne x') = x == x'
    (==) (ThatOne x) (ThatOne x') = x == x'
    (==) _ _ = False

data EitherOr a b =
    Hello a | Goodbye b
    deriving Show

instance (Eq a, Eq b) => Eq (EitherOr a b) where
    (==) (Hello x) (Hello x') = x == x'
    (==) (Goodbye x) (Goodbye x') = x == x'
    (==) _ _ = False