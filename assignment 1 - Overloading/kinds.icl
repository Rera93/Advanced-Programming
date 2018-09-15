// Brigel Pineti s1005549
// Tim lastName studentID

module kinds

import StdEnv

:: Bin a = Leaf | Bin (Bin a) a (Bin a)
:: Tree a b = Tip a | Node (Tree a b) b (Tree a b)
:: Rose a = Rose a [Rose a]
:: T1 a b = C11 (a b) | C12 b
:: T2 a b c = C2 (a (T1 b c))
:: T3 a b c = C3 (a b c)
:: T4 a b c = C4 (a (b c))

Start = "True"

