module Tests where

import Cipher
import Test.QuickCheck

alphGen :: Gen Char
alphGen = elements ['a'..'z']

wordGen :: Gen String
wordGen = listOf alphGen `suchThat` (\x -> length x > 3)

prop_caesar :: Property
prop_caesar =
    forAll (arbitrary :: Gen Int) (\i ->
        forAll wordGen (\word -> (unCaesar i (caesar i word)) == word))

prop_vigenere :: Property
prop_vigenere =
    forAll wordGen (\word ->
        forAll wordGen (\key ->
            (unVigenere key (vigenere key word)) == word))

runQC :: IO ()
runQC = do
    quickCheck prop_caesar
    quickCheck prop_vigenere