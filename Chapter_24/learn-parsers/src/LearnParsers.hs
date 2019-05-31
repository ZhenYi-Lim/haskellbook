module LearnParsers where

import Text.Trifecta

stop :: Parser a
stop = unexpected "stop"

one :: Parser Char
one = char '1'

one' :: Parser b
one' = one >> stop

two :: Parser Char
two = char '2'

oneTwo :: Parser Char
oneTwo = char '1' >> char '2'

oneTwo' :: Parser b
oneTwo' = oneTwo >> stop

testParse :: Parser Char -> IO ()
testParse p = print $ parseString p mempty "123"
--parseString :: Parser a -> Text.Trifecta.Delta.Delta -> String -> Result a
--testParse one and testParse oneTwo succeeds, but testparse two fails.

testParseEOF :: Parser () -> IO ()
testParseEOF p = print $ parseString p mempty "123"

testParseString :: Parser String -> IO ()
testParseString p = print $ parseString p mempty "123"

pNL s = putStrLn ('\n' : s)

oneEOF :: Parser ()
oneEOF = one >> eof

oneTwoEOF :: Parser ()
oneTwoEOF = oneTwo >> eof

p123 :: String -> IO ()
p123 s = print $ parseString (string s ::Parser String) mempty "123"

--copied from krikchaip, don't understand how it works.
string' :: String -> Parser String
string' []     = return mempty
string' (x:xs) = do
    c <- char x
    (c :) <$> string' xs