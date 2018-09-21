// Brigel Pineti s1005549
// Tim Turksema s1013838

module reviewQuestions

import StdEnv

:: UNIT = UNIT
:: PAIR a b = PAIR a b
:: EITHER a b = LEFT a | RIGHT b
:: CONS a = CONS String a


//1.1
//It does not matter whether we write it like this:
//instance==UNIT where (==) UNIT UNIT = True
//Or like this: 
// instance==UNIT where (==) x y = TRUE
// because we only define equality over two UNIT types. Comparing a UNIT with anything else will result in an exception, because it expects a comparison of a UNIT with a UNIT. 

//1.2
// No, we do not have to check for the first argument of the CONS, because the compiler will give an error if the types of the second argument differ.

//1.3
// They will both result into CONS UNIT, when these are compared to each other, they will be equal following the equality definition on CONS and UNIT. However, in reality they are not equal.
//:: Bin a = Leaf | Bin (Bin a) a (Bin a)
//:: BinG a :==EITHER (CONS UNIT) (CONS (PAIR (Bin a) (PAIR a (Bin a))))
//:: ListG a :==EITHER (CONS UNIT) (CONS (PAIR a [a]))

Start = (CONS "String" "1") == (CONS "Int")