import Control.Applicative
f x = lookup x [ (3, "hello"), (4, "julie"), (5, "kbai")]
g y = lookup y [ (7, "sup?"), (8, "chris"), (9, "aloha")]
h z = lookup z [(2, 3), (5, 6), (7, 8)]
m x = lookup x [(4, 10), (8, 13), (1, 9001)]

validateLength :: Int -> String -> Maybe String
validateLength maxLen s =
    if (length s) > maxLen
    then Nothing
    else Just s
newtype Name = Name String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)
mkName :: String -> Maybe Name
mkName s = fmap Name $ validateLength 25 s
mkAddress :: String -> Maybe Address
mkAddress a = fmap Address $ validateLength 100 a
data Person = Person Name Address deriving (Eq, Show)
mkPerson :: String -> String -> Maybe Person
mkPerson n a =
    case mkName n of
        Nothing -> Nothing
        Just n' ->
            case mkAddress a of
                Nothing -> Nothing
                Just a' ->
                    Just $ Person n' a'
mkPerson' :: String -> String -> Maybe Person
mkPerson' n a = Person <$> mkName n <*> mkAddress a
