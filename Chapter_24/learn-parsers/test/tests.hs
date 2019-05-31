{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Map as M
import Test.Hspec
import Text.Trifecta

import Data.Ini
import SemVer
import PositiveInteger
import PhoneNumber

main :: IO ()
main = hspec $ do
    describe "Assignment Parsing" $
        it "can parse a simple assignment" $ do
            let m = parseByteString parseAssignment mempty assignmentEx
                r' = maybeSuccess m
            print m
            r' `shouldBe` Just ("woot", "1")

    describe "Header Parsing" $
        it "can parse a simple header" $ do
        let m = parseByteString parseHeader mempty headerEx
            r' = maybeSuccess m
        print m
        r' `shouldBe` Just (Header "blah")

    describe "Comment parsing" $
        it "Skips comment before header" $ do
        let p = skipComments >> parseHeader
            i = "; woot\n[blah]"
            m = parseByteString p mempty i
            r' = maybeSuccess m
        print m
        r' `shouldBe` Just (Header "blah")

    describe "Section parsing" $
        it "can parse a simple section" $ do
        let m = parseByteString parseSection mempty sectionEx
            r' = maybeSuccess m
            states = M.fromList [("Chris", "Texas")]
            expected' = Just (Section (Header "states") states)
        print m
        r' `shouldBe` expected'

    describe "INI parsing" $
        it "Can parse multiple sections" $ do
            let m = parseByteString parseIni mempty sectionEx''
                r' = maybeSuccess m
                sectionValues = M.fromList [ ("alias", "claw"), ("host", "wikipedia.org")]
                whatisitValues = M.fromList [("red", "intoothandclaw")]
                expected' = Just (Config
                                    (M.fromList
                                    [ (Header "section"
                                    , sectionValues)
                                    , (Header "whatisit"
                                    , whatisitValues)]))
            print m
            r' `shouldBe` expected'

    describe "SemVer parsing" $
        it "Can parse SemVer" $ do
            let m1 = parseString parseSemVer mempty "2.1.1"
                r1 = maybeSuccess m1
                e1 = Just (SemVer 2 1 1 [] [])
            print m1
            r1 `shouldBe` e1
            let m2 = parseString parseSemVer mempty "1.0.0-x.7.z.92"
                r2 = maybeSuccess m2
                e2 = Just (SemVer 1 0 0 [NOSS "x",
                                         NOSI 7,
                                         NOSS "z",
                                         NOSI 92]
                                         [])
            print m2
            r2 `shouldBe` e2
            let m3 = parseString parseSemVer mempty "1.0.0-gamma+002"
                r3 = maybeSuccess m3
                e3 = Just (SemVer 1 0 0 [NOSS "gamma"] [NOSI 2])
            print m3
            r3 `shouldBe` e3
            let m4 = parseString parseSemVer mempty "1.0.0-beta+oof.sha.41af286"
                r4 = maybeSuccess m4
                e4 = Just (SemVer 1 0 0 [NOSS "beta"] [NOSS "oof",
                                                       NOSS "sha",
                                                       NOSS "41af286"])
            print m4
            r4 `shouldBe` e4
            let big = SemVer 2 1 1 [] []
                little = SemVer 2 1 0 [] []
                m5 = big > little
                e5 = True
            print "big > little == True"
            m5 `shouldBe` e5
    
    describe "PositiveInteger parsing" $
        it "Can parse positive integers" $ do
            let m1 = parseString base10Integer mempty "123"
                r1 = maybeSuccess m1
                e1 = Just 123
            print m1
            r1 `shouldBe` e1
            let m2 = parseString base10Integer' mempty "-123abc"
                r2 = maybeSuccess m2
                e2 = Just (-123)
            print m2
            r2 `shouldBe` e2

    describe "PhoneNumber parsing" $
        it "Can parse phone numbers" $ do
            let m1 = parseString parsePhone mempty "123-456-7890"
                r1 = maybeSuccess m1
                e1 = Just (PhoneNumber 123 456 7890)
            print m1
            r1 `shouldBe` e1
            let m2 = parseString parsePhone mempty "123-456-7890"
                r2 = maybeSuccess m2
                e2 = Just (PhoneNumber 123 456 7890)
            print m2
            r2 `shouldBe` e2
            let m3 = parseString parsePhone mempty "123-456-7890"
                r3 = maybeSuccess m3
                e3 = Just (PhoneNumber 123 456 7890)
            print m3
            r3 `shouldBe` e3
            let m4 = parseString parsePhone mempty "123-456-7890"
                r4 = maybeSuccess m4
                e4 = Just (PhoneNumber 123 456 7890)
            print m4
            r4 `shouldBe` e4