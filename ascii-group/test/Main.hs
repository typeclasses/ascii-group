module Main (main) where

import ASCII.Char (Char (..), allCharacters)
import ASCII.Group
import Control.Monad (Monad (..), when)
import Data.Bool (not)
import Data.Foldable (all)
import Data.Function (($))
import Data.List (filter, length)
import Hedgehog
  ( Property,
    assert,
    checkParallel,
    discover,
    property,
    withTests,
    (===),
  )
import System.Exit (exitFailure)
import System.IO (IO)

main :: IO ()
main = checkParallel $$(discover) >>= \ok -> when (not ok) exitFailure

prop_letter :: Property
prop_letter =
  withTests 1 $
    property $
      charGroup CapitalLetterA === Printable

prop_control :: Property
prop_control =
  withTests 1 $
    property $
      charGroup EndOfTransmission === Control

prop_not_printable :: Property
prop_not_printable =
  withTests 1 $
    property $
      assert $
        not $
          inGroup Printable EndOfTransmission

prop_is_control :: Property
prop_is_control =
  withTests 1 $
    property $
      assert $
        inGroup Control EndOfTransmission

-- It is perhaps surprising that space is considered a
-- "printable" character, since it does not visibly appear.
prop_space_is_printable :: Property
prop_space_is_printable =
  withTests 1 $
    property $
      charGroup Space === Printable

-- It is perhaps surprising that horizontal tab is not
-- in the same category as space.
prop_horizontal_tab_is_control :: Property
prop_horizontal_tab_is_control =
  withTests 1 $
    property $
      charGroup HorizontalTab === Control

prop_various_printables :: Property
prop_various_printables =
  withTests 1 $
    property $
      assert $
        all (inGroup Printable) [CapitalLetterA, SmallLetterZ, Digit4, Tilde]

prop_various_controls :: Property
prop_various_controls =
  withTests 1 $
    property $
      assert $
        all (inGroup Control) [Null, Substitute, UnitSeparator, Delete]

prop_count_printables :: Property
prop_count_printables =
  withTests 1 $
    property $
      length (filter (inGroup Printable) allCharacters) === 95

prop_count_controls :: Property
prop_count_controls =
  withTests 1 $
    property $
      length (filter (inGroup Control) allCharacters) === 33
