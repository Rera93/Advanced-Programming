// Brigel Pineti s1005549
// Tim Turksema s1013838

module serializeNativeGenerics

import StdEnv, StdMaybe, StdGeneric

class serialize a | read{|*|}, write{|*|} a

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

generic read a :: [String] -> Maybe (a, [String])
read{|UNIT|} list = Just (UNIT, list)
read{|PAIR|} ra rb list = case ra list of
                              Just (a, rest) -> case rb rest of
                                                    Just (b, rest`) -> Just (PAIR a b, rest`)
                                                    _               -> Nothing
                              _              -> Nothing
read{|EITHER|} ra rb list = case ra list of
                                Just (a, rest) -> Just (LEFT a, rest)
                                Nothing        -> case rb list of
                                                      Just (b, rest) -> Just (RIGHT b, rest)
                                                      Nothing        -> Nothing                              
read{|CONS of name|} f ["(":n:rest]
| n == name.gcd_name = case f rest of
	Just (x, [")":rest`]) -> Just (CONS x, rest`)
	_ -> Nothing
| otherwise = Nothing
read{|OBJECT|} r list = case r list of
                            Just (x, rest) -> Just (OBJECT x, rest)
                            _              -> Nothing
read{|Int|} [x : rest] = Just (toInt x, rest)
read{|Bool|} ["True" : rest] = Just (True, rest)
read{|Bool|} ["False" : rest] = Just (False, rest)
                                             
                     

derive write Bin, [], (,), Coin
derive read Bin, [], (,), Coin

Start = 
  [test True
  ,test False
  ,test 0
  ,test 123
  ,test -36
  ,test [42]
  ,test [0..4]
  ,test [[True],[]]
  ,test [[[1]],[[2],[3,4]],[[]]]
  ,test (Bin Leaf True Leaf)
  ,test [Bin (Bin Leaf [1] Leaf) [2] (Bin Leaf [3] (Bin Leaf [4,5] Leaf))]
  ,test [Bin (Bin Leaf [1] Leaf) [2] (Bin Leaf [3] (Bin (Bin Leaf [4,5] Leaf) [6,7] (Bin Leaf [8,9] Leaf)))]
  ,test Head
  ,test Tail
  ,test (7,True)
  ,test (Head,(7,[Tail]))
  ,["End of the tests.\n"]
  ]

test :: a -> [String] | serialize, == a
test a = 
  (if (isJust r)
    (if (fst jr == a)
      (if (isEmpty (tl (snd jr)))
        ["Oke"]
        ["Not all input is consumed! ":snd jr])
      ["Wrong result: ":write{|*|} (fst jr) []])
    ["read result is Nothing"]
  ) ++ [", write produces: ": s]
  where
    s = write{|*|} a ["\n"]
    r = read{|*|} s
    jr = fromJust r