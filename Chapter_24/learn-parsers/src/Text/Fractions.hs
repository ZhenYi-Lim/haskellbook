{-# LANGUAGE OverloadedStrings #-}

module Text.Fractions where
    
import Control.Applicative
import Data.Ratio ((%))
import Text.Trifecta
import Data.Attoparsec.Text (parseOnly)
import Data.String (IsString)

{- badFraction = "1/0"
alsoBad = "10"
shouldWork = "1/2"
shouldAlsoWork = "2/1"

parseFraction :: Parser Rational
parseFraction = do
    numerator <- decimal
    char '/'
    denominator <- decimal
    return (numerator % denominator)
    -}

badFraction :: IsString s => s
badFraction = "1/0"
alsoBad :: IsString s => s
alsoBad = "10"
shouldWork :: IsString s => s
shouldWork = "1/2"
shouldAlsoWork :: IsString s => s
shouldAlsoWork = "2/1"

parseFraction :: (Monad m, TokenParsing m) => m Rational
parseFraction = do
    numerator <- decimal
    _ <- char '/'
    denominator <- decimal
    case denominator of
        0 -> fail "Denominator cannot be zero"
        _ -> return (numerator % denominator)

virtuousFraction :: Parser Rational
virtuousFraction = do
    numerator <- decimal
    char '/'
    denominator <- decimal
    case denominator of
        0 -> fail "Denominator cannot be zero"
        _ -> return (numerator % denominator) 