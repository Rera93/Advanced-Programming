// Brigel Pineti s1005549
// Tim Turksema s1013838

module genericSerialization

import StdMaybe, StdEnv, StdList

class serialize a where
	write :: a [String] -> [String]
	read :: [String] -> Maybe (a,[String])

:: UNIT = UNIT
:: EITHER a b = LEFT a | RIGHT b
:: PAIR a b = PAIR a b
:: CONS a = CONS String a

:: List a = Nil | Cons a (List a)
:: Bin a = Leaf | Bin (Bin a) a (Bin a)

::ListG a = EITHER (CONS UNIT) (CONS (PAIR a [a]))
::BinG a = EITHER (CONS UNIT) (CONS (PAIR (Bin a) (PAIR a (Bin a))))

fromList :: [a] -> ListG a
fromList [] = LEFT (CONS "Nil" UNIT)
fromList [a:as] = RIGHT (CONS "Cons" (PAIR a as))

toList :: (ListG a) -> [a]
toList (LEFT (CONS _ UNIT)) = []
toList (RIGHT (CONS _ (PAIR a as))) = [a:as]

//fromBin :: (Bin a) -> BinG a
//toBin :: (BinG a) -> Bin a



//2.1

Start = toList (fromList [1..4])