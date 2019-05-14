module Bull
    ( Bull
    , BullMappend
    ) where

import Test.QuickCheck

data Bull =
    Fools
    | Twoo
    deriving (Eq, Show)

instance Arbitrary Bull where
    arbitrary = frequency [ (1, return Fools), (1, return Twoo) ]

instance Monoid Bull where
    mempty = Fools

instance Semigroup Bull where
    _ <> _ = Fools

type BullMappend = Bull -> Bull -> Bull -> Bool