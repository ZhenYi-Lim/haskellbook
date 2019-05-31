{-# LANGUAGE QuasiQuotes #-}

module AltParsingExercise where

import Control.Applicative
import Text.RawString.QQ
import Text.Trifecta

import Text.Fractions

type FractionOrString = Either Rational Integer

parseDecimal :: Parser Integer
parseDecimal = do
    x <- decimal
    c <- anyChar
    case c of
        '\n' -> return x
        _ -> fail "unexpected char"

parseFoD :: Parser FractionOrString
parseFoD = do
    skipMany (oneOf "\n")
    (Left <$> (try parseFraction)) <|> (Right <$> parseDecimal)

test1 :: String
test1 = "\n2/3\n1\n4/5\n2\n"

test2 :: String
test2 = "\n1/2\n23\n1/0\n"