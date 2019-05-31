{-# LANGUAGE QuasiQuotes #-}

module AltParsing where

import Control.Applicative
import Text.RawString.QQ
import Text.Trifecta

type NumberOrString = Either Integer String

a = "blah"
b = "123"
c = "123blah789"

parseNos :: Parser NumberOrString
parseNos = (Left <$> integer) <|> (Right <$> some letter)

-- doesn't seem to work with trifecta-2 (1.5.2 require GHC <8.65 (that comes with lts-13.23))
-- unable to build with trifecta-1.5.2, requires unattainable version of base.
parseNos' :: Parser NumberOrString
parseNos' = do
    skipMany (oneOf "\n")
    v <- ((Left <$> integer) <|> (Right <$> some letter))
    skipMany (oneOf "\n")
    return v

eitherOr :: String
eitherOr = [r|
    123
    abc
    456
    def
    |]

-- pasrseNos' works with this version of eitherOr
eitherOr' :: String
eitherOr' = "\n123\nabc\n456\ndef\n"