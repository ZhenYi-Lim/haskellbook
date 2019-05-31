module PhoneNumber where

import Control.Applicative
import Text.Trifecta
import Text.Parser.Combinators

type NumberingPlanArea = Int
type Exchange = Int
type LineNumber = Int

data PhoneNumber =
    PhoneNumber NumberingPlanArea Exchange LineNumber
    deriving (Eq, Show)

--123-456-7890
parsePhoneType1 :: Parser PhoneNumber
parsePhoneType1 = do
    npa <- some digit
    char '-'
    ex <- some digit
    char '-'
    ln <- some digit
    eof
    return (PhoneNumber (read npa)
                        (read ex)
                        (read ln))

--1234567980
parsePhoneType2 :: Parser PhoneNumber
parsePhoneType2 = do
    lod <- (some digit)
    return (PhoneNumber (read (take 3 lod))
                        (read (take 3 (drop 3 lod)))
                        (read (drop 6 lod)))

--(123) 456-7890
parsePhoneType3 :: Parser PhoneNumber
parsePhoneType3 = do
    char '('
    npa <- some digit
    char ')'
    ex <- some digit
    char '-'
    ln <- some digit
    eof
    return (PhoneNumber (read npa)
                        (read ex)
                        (read ln))

--1-123-456-7890
parsePhoneType4 :: Parser PhoneNumber
parsePhoneType4 = do
    char '1'
    char '-'
    npa <- some digit
    char '-'
    ex <- some digit
    char '-'
    ln <- some digit
    eof
    return (PhoneNumber (read npa)
                        (read ex)
                        (read ln))

parsePhone :: Parser PhoneNumber
parsePhone = parsePhoneType1 <|> 
             parsePhoneType2 <|> 
             parsePhoneType3 <|> 
             parsePhoneType4