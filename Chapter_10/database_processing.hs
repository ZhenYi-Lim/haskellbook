import Data.Time

data DatabaseItem =
    DbString String | DbNumber Integer | DbDate UTCTime
    deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
    [ DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
    , DbNumber 9001
    , DbString "Hello, world!"
    , DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
    ]

--foldr :: (a -> b -> b) -> b -> [a] -> b

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate xs = foldr f [] xs
    where
        f (DbDate x) z = x : z
        f _ z = z
    
filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber xs = foldr f [] xs
    where
        f (DbNumber x) z = x : z
        f _ z = z

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent xs = foldr f date dbDates
    where
        f x z = if x > z then x else z
        (date : dbDates) = filterDbDate xs

sumDb :: [DatabaseItem] -> Integer
sumDb xs = foldr (\x z -> x+z) 0 (filterDbNumber xs)

avgDb :: [DatabaseItem] -> Double
avgDb xs = fromIntegral((sumDb xs))/fromIntegral(length (filterDbNumber xs))
