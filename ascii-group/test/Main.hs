module Main (main) where

import ASCII.Group

import Test.Hspec

import ASCII.Char (Char (..), allCharacters)
import Data.Bool (Bool(..))
import Data.Foldable (all)
import Data.Function (($))
import Data.List (filter, length)
import System.IO (IO)

main :: IO ()
main = hspec $ do
  it "" $ charGroup CapitalLetterA `shouldBe` Printable
  it "" $ charGroup EndOfTransmission `shouldBe` Control
  it "" $ inGroup Printable EndOfTransmission `shouldBe` False
  it "" $ inGroup Control EndOfTransmission `shouldBe` True

  it "It is perhaps surprising that space is considered a\
     \ \"printable\" character, since it does not visibly appear." $
    charGroup Space `shouldBe` Printable

  it "It is perhaps surprising that horizontal tab is not\
     \ in the same category as space." $
    charGroup HorizontalTab `shouldBe` Control

  it "" $
    all (inGroup Printable) [CapitalLetterA, SmallLetterZ, Digit4, Tilde]
      `shouldBe` True

  it "" $
    all (inGroup Control) [Null, Substitute, UnitSeparator, Delete]
      `shouldBe` True

  it "" $ length (filter (inGroup Printable) allCharacters) `shouldBe` 95

  it "" $ length (filter (inGroup Control) allCharacters) `shouldBe` 33
