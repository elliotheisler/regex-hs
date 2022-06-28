{-# LANGUAGE OverloadedStrings #-}
{- with overloaded string literals enabled (with OverloadedStrings) a string literal has type (IsString a) => a.

This means that the usual string syntax can be used, e.g., for ByteString, Text, and other variations of string like types. String literals behave very much like integer literals -}
module HspecRETree where

import Test.Hspec
import Text.Parsec (ParseError)
import Regex
import RETree

type Result = Either ParseError RETree

spec_parseRE :: Spec
spec_parseRE = do

    it "evaluates Repetition of zero times to empty regex" $
      (parseRE "a{0,0}" :: Result) `shouldBe` (Right $ read "Epsilon" :: Result)

    it "evaluates '|' (union of nothing) to empty regex" $
      (parseRE "|" :: Result) `shouldBe` (Right $ read "Epsilon" :: Result)
