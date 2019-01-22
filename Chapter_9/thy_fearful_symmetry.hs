myWords :: String -> [String]
myWords x
    | x == [] = []
    | x == " " = []
    | (dropWhile (>' ') x) == [] = [(takeWhile (>' ') x)]
    | (takeWhile (>' ') x) == "" = (myWords (tail (dropWhile (>' ') x)))
    | otherwise = (takeWhile (>' ') x) : (myWords (tail (dropWhile (>' ') x)))

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

myLines :: String -> [String]
myLines x
    | x == [] = []
    | (dropWhile (/='\n') x) == [] = [x]--[(takeWhile (/='\n') x)]
    | otherwise = (takeWhile (/='\n') x) : (myLines (tail (dropWhile (/='\n') x)))

shouldEqual =
    [ "Tyger Tyger, burning bright"
    , "In the forests of the night"
    , "What immortal hand or eye"
    , "Could frame thy fearful symmetry?"
    ]
    
result = ((myLines sentences) == shouldEqual)

myGeneral :: String -> Char -> [String]
myGeneral x c
    | x == [] = []
    | (dropWhile (/=c) x) == [] = [x]--[(takeWhile (/='\n') x)]
    | otherwise = (takeWhile (/=c) x) : (myGeneral (tail (dropWhile (/=c) x) ) c )