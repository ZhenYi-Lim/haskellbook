module SemVer where

import Control.Applicative
import Text.Trifecta
import Text.Parser.Combinators
import Data.Monoid

data NumberOrString = NOSS String | NOSI Integer deriving (Eq, Show)

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]
data SemVer = SemVer Major Minor Patch Release Metadata deriving (Eq, Show)

parseNOS :: Parser NumberOrString
parseNOS = (NOSI <$> try (decimal <* notFollowedBy letter)) <|>
           (NOSS <$> some (letter <|> digit))

parsePreRelease :: Parser Release
parsePreRelease = try (char '-' *> sepBy1 parseNOS (char '.')) <|> mempty

parseMetadata :: Parser Metadata
parseMetadata = try (char '+' *> sepBy1 parseNOS (char '.')) <|> mempty

parseSemVer :: Parser SemVer
parseSemVer = do
    major <- decimal
    char '.'
    minor <- decimal
    char '.'
    patch <- decimal
    release <- parsePreRelease
    metadata <- parseMetadata
    eof
    return (SemVer major minor patch release metadata)

instance Ord NumberOrString where
    compare (NOSI _) (NOSS _) = GT
    compare (NOSS _) (NOSI _) = LT
    compare (NOSI x) (NOSI y) = compare x y
    compare (NOSS x) (NOSS y) = compare x y

compareRelease :: Release -> Release -> Ordering
compareRelease [] [] = EQ
compareRelease [] _ = GT
compareRelease _ [] = LT
compareRelease x y = compare x y

instance Ord SemVer where
    compare (SemVer mjx mnx px prx _) (SemVer mjy mny py pry _) =
        compare mjx mjy <>
        compare mnx mny <>
        compare px py <>
        compareRelease prx pry