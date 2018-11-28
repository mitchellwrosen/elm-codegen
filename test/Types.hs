module Types where

data T1
data T2 = T2
newtype T3 = T3 ()
data T4 = T4 Int Bool String
data T5 = T5_1 | T5_2 | T5_3
data T6 a = T6
data T7 foo = T7
data T8 a = T8 a
data T9 a = T9 a a
data T10 a = T10_1 | T10_2 a
data T11 = T11 [Int]
data T12 a = T12 [a]
data T13 = T13 (Int, Int)
data T14 = T14 (Int, Int, Int)
data T15 = T15 (Int -> Bool)
data T16 = T16 (Int -> Bool -> Int)
data T17 = T17 { t17 :: () }
