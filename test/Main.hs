{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}

import Control.Monad
import Language.Elm.Codegen
import Types

main :: IO ()
main = do
  $(fromName ''T1) === ElmDecDataType (ElmDataType "T1" [] [])
  $(fromName ''T2) === ElmDecDataType (ElmDataType "T2" [] [ElmConstructor "T2" []])
  $(fromName ''T3) === ElmDecDataType (ElmDataType "T3" [] [ElmConstructor "T3" [ElmTypeUnit]])
  $(fromName ''T4) === ElmDecDataType (ElmDataType "T4" [] [ElmConstructor "T4" [ElmTypeCon "Int" [], ElmTypeCon "Bool" [], ElmTypeCon "String" []]])
  $(fromName ''T5) === ElmDecDataType (ElmDataType "T5" [] [ElmConstructor "T5_1" [], ElmConstructor "T5_2" [], ElmConstructor "T5_3" []])
  $(fromName ''T6) === ElmDecDataType (ElmDataType "T6" ["a"] [ElmConstructor "T6" []])
  $(fromName ''T7) === ElmDecDataType (ElmDataType "T7" ["foo"] [ElmConstructor "T7" []])
  $(fromName ''T8) === ElmDecDataType (ElmDataType "T8" ["a"] [ElmConstructor "T8" [ElmTypeVar "a"]])
  $(fromName ''T9) === ElmDecDataType (ElmDataType "T9" ["a"] [ElmConstructor "T9" [ElmTypeVar "a", ElmTypeVar "a"]])
  $(fromName ''T10) === ElmDecDataType (ElmDataType "T10" ["a"] [ElmConstructor "T10_1" [], ElmConstructor "T10_2" [ElmTypeVar "a"]])
  $(fromName ''T11) === ElmDecDataType (ElmDataType "T11" [] [ElmConstructor "T11" [ElmTypeCon "List" [ElmTypeCon "Int" []]]])
  $(fromName ''T12) === ElmDecDataType (ElmDataType "T12" ["a"] [ElmConstructor "T12" [ElmTypeCon "List" [ElmTypeVar "a"]]])
  $(fromName ''T13) === ElmDecDataType (ElmDataType "T13" [] [ElmConstructor "T13" [ElmTypeTuple (ElmTypeCon "Int" []) (ElmTypeCon "Int" []) Nothing]])
  $(fromName ''T14) === ElmDecDataType (ElmDataType "T14" [] [ElmConstructor "T14" [ElmTypeTuple (ElmTypeCon "Int" []) (ElmTypeCon "Int" []) (Just (ElmTypeCon "Int" []))]])
  $(fromName ''T15) === ElmDecDataType (ElmDataType "T15" [] [ElmConstructor "T15" [ElmTypeLambda (ElmTypeCon "Int" []) (ElmTypeCon "Bool" [])]])
  $(fromName ''T16) === ElmDecDataType (ElmDataType "T16" [] [ElmConstructor "T16" [ElmTypeLambda (ElmTypeCon "Int" []) (ElmTypeLambda (ElmTypeCon "Bool" []) (ElmTypeCon "Int" []))]])
  $(fromName ''T17) === ElmDecDataType (ElmDataType "T17" [] [ElmConstructor "T17" [ElmTypeRecord [("t17", ElmTypeUnit)] Nothing]])
  $(fromName ''T18) === ElmDecDataType (ElmDataType "T18" [] [ElmConstructor "T18" [ElmTypeRecord [("t18_1", ElmTypeCon "Int" []), ("t18_2", ElmTypeCon "Bool" [])] Nothing]])
  $(fromName ''T19) === ElmDecTypeAlias (ElmTypeAlias "T19" [] ElmTypeUnit)
  $(fromName ''T20) === ElmDecDataType (ElmDataType "T20" [] [ElmConstructor "T20" [ElmTypeRecord [("t20", ElmTypeCon "Int" [])] Nothing]])
  $(fromName ''T21) === ElmDecDataType (ElmDataType "T21" ["a"] [ElmConstructor "T21" [ElmTypeRecord [("t21", ElmTypeVar "a")] Nothing]])

(===) :: (Eq a, Show a) => a -> a -> IO ()
actual === expected =
  unless (actual == expected)
    (fail ("`" ++ show expected ++ "' /= `" ++ show actual ++ "'"))
