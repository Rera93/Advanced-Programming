// Brigel Pineti s1005549
// Tim Turksema s1013838

module serialize3Start

/*
  Definitions for assignment 3 in AFP 2018
  Kind indexed gennerics
  Pieter Koopman, pieter@cs.ru.nl
  September 2018
  
  Use StdEnv or iTask environment.
  Use Basic Values Only as conclose option for a nicer output.
*/

import StdEnv, StdMaybe

:: Write a :== a [String] -> [String]
:: Read a  :== [String] -> Maybe (a, [String])

// use this as serialize0 for kind *
class serialize a where
  write :: a [String] -> [String]
  read  :: [String] -> Maybe (a,[String])

// ---

class serialize1 t where
  write1 :: (Write a) (t a) [String] -> [String]
  read1  :: (Read a) [String] -> Maybe (t a,[String])
  
class serialize2 t where
  write2 :: (Write a) (Write b) (t a b) [String] -> [String]
  read2  :: (Read a) (Read b) [String] -> Maybe (t a b,[String])

instance serialize Bool where
  write b c = [toString b:c]
  read list = foldl (match list) Nothing [True, False]
	where
		match [string: rest] r bool | toString bool == string
				= Just (bool, rest)
				= r
		match _ r bool = r

//instance serialize2 EITHER where 
// write2 wa wb (LEFT a) c = wa a c
// write2 wa wb (RIGHT b) c = wb b c
// read ra rb l = case ra l of 
// 	Just (left, rest) -> Just (LEFT left, rest)
// 	Nothing 		-> case rb l of
// 		Just (right, `rest) -> Just (RIGHT right, `rest)
// 		Nothing -> Nothing     
 	

instance serialize Int where
  write i c = [toString i:c]
  read list = foldl (match list) Nothing [True, False]
  where
    match [string: rest] r bool
      # int = toInt string
      | string == toString int
        = Just (int, rest)
        = r
    match _ r bool = r
    
instance serialize String where
	write s ss  = [s:ss]
	read [s:ss] = Just (s, ss)
	read _      = Nothing

// ---

:: UNIT     = UNIT
:: EITHER a b = LEFT a | RIGHT b
:: PAIR   a b = PAIR a b
:: CONS   a   = CONS String a

// ---

instance serialize UNIT where
    write UNIT list = list
    read list       = Just (UNIT, list)
    
instance serialize2 EITHER where
    write2 f g (LEFT x) list  = f x list
    write2 f g (RIGHT y) list = g y list
    read2 f g list            = case f list of
                                    Just (x, rest) -> Just (LEFT x, rest)
                                    Nothing       -> case g list of
                                                         Just (y, rest) -> Just (RIGHT y, rest)
                                                         Nothing        -> Nothing 
                                                         
instance serialize2 PAIR where
    write2 f g (PAIR x y) list = f x (g y list)
    read2 f g list             = case f list of
                                     Just (x, rest) -> case g list of
                                                           Just (y, rest`) -> Just ((PAIR x y), rest`)
                                                           _               -> Nothing
                                     _              -> Nothing
                                     
instance serialize1 CONS where
    write1 f (CONS name x) list = write name (write "(" (f x [")" : list]))
    read1 f [name : "(" : list] = case f list of
                                      Just (x, rest) -> Just ((CONS name x), rest)
                                      _              -> Nothing
    read1 f _                   = Nothing
                                     

:: ListG a :== EITHER (CONS UNIT) (CONS (PAIR a [a]))

fromList :: [a] -> ListG a
fromList []    = LEFT  (CONS NilString  UNIT)
fromList [a:x] = RIGHT (CONS ConsString (PAIR a x))

toList :: (ListG a) -> [a]
toList (LEFT  (CONS NilString  UNIT))       = []
toList (RIGHT (CONS ConsString (PAIR a x))) = [a:x]

NilString  :== "Nil"
ConsString :== "Cons"

class serializeCONS a where
    writeCons :: (Write a) (CONS a) [String] -> [String]
    readCons  :: String (Read a) [String] -> Maybe (CONS a, [String])
    
instance serializeCONS a where
  writeCons w (CONS cons a) c = ["(":cons:w a [")":c]]
  readCons c ra ["(":cons:s] 
    | c == cons = case ra s of
      Just (a, [")":s]) = Just(CONS cons a, s)
      _ = Nothing
      = Nothing
  readCons _ _ _ = Nothing

instance serializeCONS UNIT where
    writeCons wa (CONS name x) list = [name : list]
    readCons name ra [n : rest] | name == n
        = Just (CONS n UNIT, rest)
    readCons _ _ _ = Nothing   

instance serialize [a] | serialize a where
    write a list = write1 write a list			
    read list    = read1 read list
 
instance serialize1 [] where
    write1 w a list = write2 (writeCons write) (writeCons (write2 w (write1 w))) (fromList a) list 
    read1 r list    = case read2 (readCons NilString read) (readCons ConsString (read2 r (read1 r))) list of
                          Just (r`, rest) -> Just (toList r`, rest)
                          Nothing         -> Nothing


// ---

:: Bin a = Leaf | Bin (Bin a) a (Bin a)

:: BinG a :== EITHER (CONS UNIT) (CONS (PAIR (Bin a) (PAIR a (Bin a))))

fromBin :: (Bin a) -> BinG a
fromBin Leaf = LEFT (CONS LeafString UNIT)
fromBin (Bin l a r) = RIGHT (CONS BinString (PAIR l (PAIR a r)))

toBin :: (BinG a) -> Bin a
toBin (LEFT (CONS _ UNIT)) = Leaf
toBin (RIGHT (CONS _ (PAIR l (PAIR a r)))) = Bin l a r

LeafString :== "Leaf"
BinString  :== "Bin"

instance == (Bin a) | == a where
  (==) Leaf Leaf = True
  (==) (Bin l a r) (Bin k b s) = l == k && a == b && r == s
  (==) _ _ = False

instance serialize (Bin a) | serialize a where	// to be improved
	write b s = s
	read    l = Nothing

// ---

:: Coin = Head | Tail
:: CoinG :== EITHER (CONS UNIT) (CONS UNIT)

fromCoin :: Coin -> CoinG
fromCoin Head = LEFT (CONS "Head" UNIT)
fromCoin Tail = RIGHT (CONS "Tail" UNIT)

toCoin :: CoinG -> Coin
toCoin (LEFT (CONS _ UNIT)) = Head
toCoin (RIGHT (CONS _ UNIT)) = Tail

instance == Coin where
  (==) Head Head = True
  (==) Tail Tail = True
  (==) _    _    = False

instance serialize Coin where
	write c s = s
	read    l = Nothing

/*
	Define a special purpose version for this type that writes and reads
	the value (7,True) as ["(","7",",","True",")"]
*/
instance serialize (a,b) | serialize a & serialize b where
	write (a,b) c = c
	read _ = Nothing

// ---
// output looks nice if compiled with "Basic Values Only" for console in project options
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
      ["Wrong result: ":write (fst jr) []])
    ["read result is Nothing"]
  ) ++ [", write produces: ": s]
  where
    s = write a ["\n"]
    r = read s
    jr = fromJust r
