import Data.Char

isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf _ [] = False
isSubseqOf [] _ = True
isSubseqOf sub seq =
    if (head sub) == (head seq)
        then isSubseqOf (tail sub) (tail seq)
    else isSubseqOf sub (tail seq)

isSubseqOf' :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf' _ [] = False
isSubseqOf' [] _ = True
isSubseqOf' xxs@(x:xs) (y:ys) =
    if x == y
        then isSubseqOf' xs ys
    else isSubseqOf' xxs ys

capitalizeWords :: String -> [(String, String)]
capitalizeWords sentence = map (\x -> (x, toUpper(head x) : (tail x) ) ) wordList
    where
        wordList = words sentence

capitalizeWords' :: String -> [(String, String)]
capitalizeWords' sentence = map (\xxs@(x:xs) -> (xxs, toUpper(x) : xs ) ) wordList
    where
        wordList = words sentence

capitalizeWord :: String -> String
capitalizeWord (x:xs)= toUpper(x):xs

--says non-exhaustive pattern matching.
{- capitalizeParagraph :: String -> String
capitalizeParagraph paragraph = go 1 paragraph ""--result
    where
        --(_,_,result) = go 1 paragraph ""
        go dot xxs@(x:xs) yys@(y:ys)
            | xxs == "" = yys
            | x == '.' = go 1 xs (yys ++ [x])
            | (dot == 1) && (isAlpha x) = go 0 xs ( yys ++ [(toUpper x)] )
            | otherwise = go dot xs (yys ++ [x]) -}

capitalizeParagraph' :: String -> String
capitalizeParagraph' paragraph = go 1 paragraph ""
    where
        go dot remainder result
            | remainder == "" = result
            | (head remainder) == '.' = go 1 (tail remainder) (result ++ [head remainder])
            | (dot == 1) && (isAlpha (head remainder)) = go 0 (tail remainder) ( result ++ [(toUpper (head remainder))] )
            | otherwise = go dot (tail remainder) (result ++ [head remainder])

capitalizeParagraph'' :: String -> String
capitalizeParagraph'' paragraph = go 1 paragraph ""--result
    where
        go _ "" result = result
        go 1 (x:xs) result
            | (isAlpha x) = go 0 xs (result ++ [toUpper x])
            | otherwise = go 1 xs (result ++ [x])
        go i (x:xs) result
            | x == '.' = go 1 xs (result ++ [x])
            | otherwise = go i xs (result ++ [x])

data DaPhone = DaPhone [(Char, String)]
phone = DaPhone [('*', "^")
                , ('#', ".,")
                , ('0', "+_")
                , ('1', "")
                , ('2', "abc")
                , ('3', "def")
                , ('4', "ghi")
                , ('5', "jkl")
                , ('6', "mno")
                , ('7', "pqrs")
                , ('8', "tuv")
                , ('9', "wxyz")]

convo :: [String]
convo =
    ["Wanna play 20 questions",
    "Ya",
    "U 1st haha",
    "Lol ok. Have u ever tasted alcohol",
    "Lol ya",
    "Wow ur cool haha. Ur turn",
    "Ok. Do u think I am pretty Lol",
    "Lol ya",
    "Just making sure rofl ur turn"]
                
type Digit = Char
type Presses = Int

{- reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps (DaPhone phoneList) char
    where
        go pps@(p:ps) char presses
            | (isUpper char) = go pps (toLower char) "*"
            | elem char (snd p) = 

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead = undefined -}