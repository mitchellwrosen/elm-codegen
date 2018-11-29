{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}

import Control.Monad
import Data.Text (Text, stripEnd, unpack)
import Lib
import NeatInterpolation
import Types

main :: IO ()
main = do
  run $(declareElmType ''T1) [text|
    type T1 |]

  run $(declareElmType ''T2) [text|
    type T2 =
      T2 |]

  run $(declareElmType ''T3) [text|
    type T3 =
      T3 () |]

  run $(declareElmType ''T4) [text|
    type T4 =
      T4 Int Bool String |]

  run $(declareElmType ''T5) [text|
    type T5 =
      T5_1
      | T5_2
      | T5_3 |]

  run $(declareElmType ''T6) [text|
    type T6 a =
      T6 |]

  run $(declareElmType ''T7) [text|
    type T7 foo =
      T7 |]

  run $(declareElmType ''T8) [text|
    type T8 a =
      T8 a |]

  run $(declareElmType ''T9) [text|
    type T9 a =
      T9 a a |]

  run $(declareElmType ''T10) [text|
    type T10 a =
      T10_1
      | T10_2 a |]

  run $(declareElmType ''T11) [text|
    type T11 =
      T11 (List Int) |]

  run $(declareElmType ''T12) [text|
    type T12 a =
      T12 (List a) |]

  run $(declareElmType ''T13) [text|
    type T13 =
      T13 ( Int, Int ) |]

  run $(declareElmType ''T14) [text|
    type T14 =
      T14 ( Int, Int, Int ) |]

  run $(declareElmType ''T15) [text|
    type T15 =
      T15 (Int -> Bool) |]

  run $(declareElmType ''T16) [text|
    type T16 =
      T16 (Int -> Bool -> Int) |]

  run $(declareElmType ''T17) [text|
    type T17 =
      T17 { t17 : () } |]

  run $(declareElmTypeAlias ''T17) [text|
    type alias T17 =
      { t17 : () } |]

  run $(declareElmType ''T18) [text|
    type T18 =
      T18 { t18_1 : Int
          , t18_2 : Bool
          } |]

  run $(declareElmTypeAlias ''T18) [text|
    type alias T18 =
      { t18_1 : Int, t18_2 : Bool } |]

  run $(declareElmType ''T19) [text|
    type T19 =
      T19 () |]

  run $(declareElmTypeAlias ''T19) [text|
    type alias T19 =
      () |]

  run $(declareElmType ''T20) [text|
    type T20 =
      T20 { t20 : Int } |]

  run $(declareElmTypeAlias ''T20) [text|
    type alias T20 =
      { t20 : Int } |]

run :: String -> Text -> IO ()
run actual expected =
  unless (actual == unpack (stripEnd expected))
    (fail actual)
