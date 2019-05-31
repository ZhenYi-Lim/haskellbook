{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Text.Trifecta
import Data.Attoparsec.Text (parseOnly)
import Data.Aeson

import LearnParsers
import Text.Fractions
import UnitOfSuccess
import AltParsing
import AltParsingExercise
import Data.Ini
import BT
import Marshalling
import SemVer
import PositiveInteger

main :: IO ()
main = do
    pNL "stop:"
    testParse stop
    pNL "one:"
    testParse one
    pNL "one':"
    testParse one'
    pNL "oneTwo:"
    testParse oneTwo
    pNL "oneTwo':"
    testParse oneTwo'
    pNL "oneEOF"
    testParseEOF oneEOF
    pNL "oneTwoEOF"
    testParseEOF oneTwoEOF
    pNL "p123 1"
    p123 "1"
    pNL "p123 12"
    p123 "12"
    pNL "p123 123"
    p123 "123"
    pNL "parseFraction"
    let parseFraction' = parseString parseFraction mempty
    print $ parseFraction' shouldWork
    print $ parseFraction' shouldAlsoWork
    print $ parseFraction' alsoBad
    --print $ parseFraction' badFraction
    pNL "virtuousFraction"
    let virtuousFraction' = parseString virtuousFraction mempty
    print $ virtuousFraction' badFraction
    print $ virtuousFraction' alsoBad
    print $ virtuousFraction' shouldWork
    print $ virtuousFraction' shouldAlsoWork
    pNL "UnitOfSuccess"
    print $ parseString (myFunc) mempty "123"
    print $ parseString (myFunc) mempty "123abc"
    pNL "AltParsing"
    let p f i = parseString f mempty i
    print $ p (some letter) a
    print $ p integer b
    print $ p parseNos a
    print $ p parseNos b
    print $ p (many parseNos) c
    print $ p (some parseNos) c
    pNL "AltParsing eitherOr"
    print $ p parseNos eitherOr
    print $ p (some (token parseNos')) eitherOr
    pNL "AltParsing eitherOr'"
    print $ p parseNos eitherOr'
    print $ p (some (token parseNos')) eitherOr'
    pNL "AltParsingExercise"
    print $ p (some (token parseFoD)) test1
    --print $ p (some (token parseFoD)) test2
    pNL "Ini"
    print $ parseByteString parseIni mempty sectionEx
    print $ parseByteString parseSection mempty sectionEx
    pNL "parseOnly is Attoparsec"
    let attoP = parseOnly parseFraction
    print $ attoP badFraction
    print $ attoP shouldWork
    print $ attoP shouldAlsoWork
    print $ attoP alsoBad
    pNL "parseString is Trifecta"
    let p f i = parseString f mempty i
    print $ p parseFraction badFraction
    print $ p parseFraction shouldWork
    print $ p parseFraction shouldAlsoWork
    print $ p parseFraction alsoBad
    pNL "trifecta"
    trifP nobackParse "13"
    trifP tryParse "13"
    trifP tryAnnot "13"
    pNL "parsec"
    parsecP nobackParse "13"
    parsecP tryParse "13"
    parsecP tryAnnot "13"
    pNL "attoparsec"
    attoP' nobackParse "13"
    attoP' tryParse "13"
    attoP' tryAnnot "13"
    pNL "Marshalling"
    let d :: Maybe TestData
        d = decode sectionJson
    print d
    print $ dec "blah"
    print $ eitherDec "blah"
    print $ dec "123"
    print $ dec "\"blah\""