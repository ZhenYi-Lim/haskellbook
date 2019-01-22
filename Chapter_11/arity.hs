-- nullary
data Example0 = Example0
    deriving (Eq, Show)

-- unary
data Example1 = Example1 Int
    deriving (Eq, Show)

-- product of Int and String
data Example2 = Example2 Int String
    deriving (Eq, Show)
