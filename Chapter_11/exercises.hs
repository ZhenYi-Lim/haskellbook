import Data.Char
import Data.List

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

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps (DaPhone phoneList) char = go phoneList char [] 0
    where
        go pps@(p:ps) char output count
            --if is space ' ', add '0' press
            | char == ' ' = [('0', 1)]
            --if is upper character, add "*" to capitalise
            | isUpper char = go pps (toLower char) [('*', 1)] 0
            --if have exhausted that digit, continue searching in the rest of the digits
            | snd p == "" = go ps char output 0
            --if matches character, return final result, with a '1' press at the end to avoid character rollover
            | char == (head (snd p)) = output++[(fst p, (count+1)), ('1', 1)]
            --otherwise increment press count by 1 and check the remaining characters of the digit
            | otherwise = go (((fst p), tail (snd p)):ps) char output (count+1)

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead (DaPhone phoneList) s = go (DaPhone phoneList) s []
    where
        go phoneList "" output = output
        go phoneList s output = go phoneList (tail s) (output++(reverseTaps phoneList (head s)))

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps input = go input 0
    where
        go [] count = count
        go (i:is) count = go is (count + (snd i))

mostPopularLetter :: String -> Char
mostPopularLetter s = go s [] []
    where
        go "" lochar locount = (lochar!!ind)
            where
                Just ind = findIndex (==(maximum locount)) locount
        go (c:cs) lochar locount
            | c == ' ' = go cs lochar locount
            | notElem c lochar = go cs (c:lochar) (1:locount)
            | otherwise = go cs lochar ((take ind locount)++[(locount!!ind)+1]++(drop (ind+1) locount))
                where
                    Just ind = findIndex (==c) lochar

coolestLtr :: [String] -> Char
coolestLtr los = mostPopularLetter (concat los)

coolestWord :: [String] -> String
coolestWord los = go ilow [] []
    where
        go [] lowc locount = (lowc!!ind)
            where
                Just ind = findIndex (==(maximum locount)) locount
        go (w:ws) lowc locount
            | notElem w lowc = go ws (w:lowc) (1:locount)
            | otherwise = go ws lowc ((take ind locount)++[(locount!!ind)+1]++(drop (ind+1) locount))
                where
                    Just ind = findIndex (==w) lowc
        ilow = concat (map words los)

data Expr
    = Lit Integer
    | Add Expr Expr

eval :: Expr -> Integer
eval (Lit x) = x
eval (Add e1 e2) = (eval e1) + (eval e2)

printExpr :: Expr -> String
printExpr (Lit x) = show x
printExpr (Add e1 e2) = (printExpr e1)++" + "++(printExpr e2)