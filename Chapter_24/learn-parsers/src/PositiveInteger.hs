module PositiveInteger where

import Control.Applicative
import Text.Trifecta
import Text.Parser.Combinators

parseDigit :: Parser Char
parseDigit = oneOf ['1'..'9']

base10Integer :: Parser Integer
base10Integer = do
    str <- (some parseDigit)
    return (read str)

base10Integer' :: Parser Integer
base10Integer' = do
    str <- try (char '-' *> (('-':) <$> (some parseDigit))) <|> (some parseDigit)
    return (read str)