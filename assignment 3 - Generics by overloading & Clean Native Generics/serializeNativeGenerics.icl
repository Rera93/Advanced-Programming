// Brigel Pineti s1005549
// Tim Turksema s1013838

module serializeNativeGenerics

import StdEnv, StdMaybe, StdGeneric

class serialize a | /*read{|*|},*/ write{|*|} a

:: Bin a = Leaf | Bin (Bin a) a (Bin a)
:: Coin  = Head | Tail

instance == (Bin a) | == a
where
	(==) Leaf Leaf = True
	(==) (Bin l a r) (Bin k b s) = l == k && a == b && r == s
	(==) _ _ = False

instance == Coin
where
	(==) Head Head = True
	(==) Tail Tail = True
	(==) _    _    = False
	
generic write a :: a [String] -> [String]
write{|UNIT|} UNIT list = list
write{|PAIR|} wa wb (PAIR a b) list = wa a (wb b list)
write{|EITHER|} wa _ (LEFT a) list = wa a list
write{|EITHER|} _ wb (RIGHT b) list = wb b list
write{|CONS of name|} w (CONS x) list = ["(" : name.gcd_name : w x [")" : list]]
write{|OBJECT|} w (OBJECT x) list = w x list
write{|Int|} x list = [toString x : list]
write{|Bool|} True list = ["True" : list]
write{|Bool|} False list = ["False" : list]
write{|String|} s list = [s : list]

derive write Bin, [], (,), Coin

Start = 1