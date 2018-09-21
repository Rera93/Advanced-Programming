// Brigel Pineti s1005549
// Tim Turksema s1013838

module reviewQuestions

import StdEnv

:: UNIT = UNIT
:: PAIR a b = PAIR a b
:: EITHER a b = LEFT a | RIGHT b
:: CON a = CON String a


//1
//instance==UNIT where (==) UNIT UNIT = TRUE

//2
//instance== (CONS a) |==a where (==) (CONS _ x) (CONS _ y) = x==y

//3
//:: Bin a = Leaf | Bin (Bin a) a (Bin a)
//:: BinG a :==EITHER (CONS UNIT) (CONS (PAIR (Bin a) (PAIR a (Bin a))))
//:: ListG a :==EITHER (CONS UNIT) (CONS (PAIR a [a]))
