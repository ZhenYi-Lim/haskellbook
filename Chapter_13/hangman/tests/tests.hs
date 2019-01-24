module Tests where

import Test.QuickCheck

--data Puzzle = Puzzle String [Maybe Char] [Char] Int

alphGen :: Gen Char
alphGen = elements ['a'..'z']

wordGen :: Gen String
wordGen = listOf alphGen `suchThat` (\x -> length x > 3)

--prop_fillInCharacter :: 

runQC :: IO ()
runQC = do
    print "hello"
    --quickCheck prop_halfIdentity