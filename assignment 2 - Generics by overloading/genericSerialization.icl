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

//Found the error, it was using syntax ListG a = instead of ListG a :==  (Delete comment when you see it)
:: Bin a = Leaf | Bin (Bin a) a (Bin a)
:: ListG a :== EITHER (CONS UNIT) (CONS (PAIR a [a]))
:: BinG a :== EITHER (CONS UNIT) (CONS (PAIR (Bin a) (PAIR a (Bin a))))

fromList :: [a] -> ListG a
fromList [] = LEFT (CONS "Nil" UNIT)
fromList [a:as] = RIGHT (CONS "Cons" (PAIR a as))

toList :: (ListG a) -> [a]
toList (LEFT (CONS _ UNIT)) = []
toList (RIGHT (CONS _ (PAIR a as))) = [a:as]

fromBin :: (Bin a) -> BinG a
fromBin Leaf        = LEFT (CONS "Leaf" UNIT)
fromBin (Bin l m r) = RIGHT (CONS "Bin" (PAIR l (PAIR m r))) 

toBin :: (BinG a) -> Bin a
toBin (LEFT (CONS _ UNIT))                 = Leaf
toBin (RIGHT (CONS _ (PAIR l (PAIR m r)))) = Bin l m r  

// 2.1 (with generic info) 

instance serialize Int where
    write b c    = [toString b:c]
    read [n : r] = Just (toInt n, r)
    read _       = Nothing  
    
instance serialize String where
	write s ss  = [s:ss]
	read [s:ss] = Just (s, ss)
	read _      = Nothing

/* Commented to execute exercise 2.2 (without generic info) 

instance serialize UNIT where
    write _ list      = ["UNIT" : list]
    read ["UNIT" : list] = Just (UNIT, list)
    read _               = Nothing
    
instance serialize (PAIR a b) | serialize a & serialize b where
    write (PAIR x y) list = ["(PAIR" : write x (write y [")" : list])]
    read ["(PAIR" : rest] = case read rest of
                               Nothing          -> Nothing
                               Just (x`, rest`) -> case read rest` of
                                                     Nothing                  -> Nothing
                                                     Just (y`,[")" : rest``]) -> Just ((PAIR x` y`), rest``)
    read _                = Nothing  
    
instance serialize (EITHER a b) | serialize a & serialize b where
    write (LEFT a) list    = ["(LEFT" : write a [")" : list]]
    write (RIGHT b) list   = ["(RIGHT" : write b [")" : list]]
    read ["(LEFT" : rest]  = case read rest of 
                                 Nothing                 -> Nothing 
                                 Just (x, [")" : rest`]) -> Just (LEFT x, rest`)
    read ["(RIGHT" : rest] = case read rest of
                                 Nothing                 -> Nothing
                                 Just (y, [")" : rest`]) -> Just (RIGHT y, rest`)
    read _                 = Nothing
    
instance serialize (CONS a) | serialize a where
    write (CONS name x) list = ["(CONS" : write name (write x [")" : list])]
    read ["(CONS" : rest]    = case read rest of 
                                   Nothing            -> Nothing
                                   Just (naam, rest`) -> case read rest` of
                                                             Nothing                  -> Nothing
                                                             Just (x, [")" : rest``]) -> Just ((CONS naam x), rest``) 
    read _                   = Nothing

 */ 
     
instance serialize [a] | serialize a where
    write l list = write (fromList l) list
    read list    = case read list of
                        Nothing         -> Nothing
                        Just (l`, rest) -> Just (toList l`, rest)
                       
instance serialize (Bin a) | serialize a where
    write l list = write (fromBin l) list
    read list    = case read list of
                       Nothing         -> Nothing 
                       Just (l`, rest) -> Just (toBin l`, rest)                   
                     
// 2.2 (without generic info)

instance serialize UNIT where
    write _ list = list
    read list    = Just (UNIT, list)
    
instance serialize (PAIR a b) | serialize a & serialize b where 
    write (PAIR x y) list = write x (write y list) 
    read list             = case read list of
                                Just (x, rest)  -> case read rest of
                                                       Just (y, rest`) -> Just ((PAIR x y), rest`)
                                                       _ -> Nothing
                                _ -> Nothing
    
instance serialize (EITHER a b) | serialize a & serialize b where
    write (LEFT x) list  = write x list
    write (RIGHT y) list = write y list
    read list            = case read list of
                               Just (x, rest) -> Just ((LEFT x), rest)
                               Nothing        -> case read list of
                                                     Just (y, rest) -> Just ((RIGHT y), rest)
                                                     Nothing        -> Nothing
      
instance serialize (CONS a) | serialize a where
    write (CONS name x) c = ["(", name : write x [")":c]]
	read ["(", name:rest] = case read rest of
		                        Just (x, [")":rest`]) -> Just (CONS name x, rest`)
		                        _ -> Nothing
	read _                = Nothing
                     
// Define equality for (Bin a) in order to test it
    
instance == (Bin a) | == a where
    == Leaf Leaf = True
    == (Bin left1 mid1 right1) (Bin left2 mid2 right2) = left1 == left2 && mid1 == mid2 && right1 == right2
    == _ _ = False  
    
test :: a -> ([String],[String]) | serialize, == a
test a = 
  (if (isJust r)
    (if (fst jr == a)
      (if (isEmpty (tl (snd jr)))
        ["Oke "]
        ["Fail: not all input is consumed! ":snd jr])
      ["Fail: Wrong result ":write (fst jr) []])
    ["Fail: read result is Nothing "]
  , ["write produces ": s]
  )
  where
    s = write a ["\n"]
    r = read s
    jr = fromJust r


//Start = write (CONS "rera" 2) ["1", "3"]
//Start = write (PAIR 1 2) ["3", "4"]
Start =[ test (Bin Leaf 5 Leaf), 
         test [1, 2, 3],
         test (Bin Leaf 5 (Bin Leaf 3 Leaf)),
         test ([[1, 2], []]),
         test (Bin (Bin Leaf 3 Leaf) 4 Leaf)
       ]
