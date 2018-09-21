// Brigel Pineti s1005549
// Tim Turksema s1013838

module genericSerialization

import StdMaybe, StdEnv, StdList

class serialize a where
	write :: a [String] ? [String]
	read :: [String] ? Maybe (a,[String])

:: UNIT = UNIT
:: EITHER a b = LEFT a | RIGHT b
:: PAIR a b = PAIR a b
:: CONS a = CONS String a

fromList :: [a] ? ListG a
toList :: (ListG a) ? [a]
fromBin :: (Bin a) ? BinG a
toBin :: (BinG a) ? Bin a