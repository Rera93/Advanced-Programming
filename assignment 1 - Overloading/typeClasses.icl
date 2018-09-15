// Brigel Pineti s1005549
// Tim Turksema s1013838

module typeClasses

import StdMaybe, StdString, StdBool, StdEnv, StdInt, StdList, StdChar, StdTuple, StdOverloaded

class serialize a where
    write :: a [String] -> [String]
    read  :: [String] -> Maybe (a, [String])
    
instance serialize Bool where
    write b c = [toString b:c]
    read ["True"  : r] = Just (True, r)
    read ["False" : r] = Just (False, r)
    read _             = Nothing
    
instance serialize Int where
    write b c    = [toString b:c]
    read [n : r] = Just (toInt n, r)
    read _       = Nothing
    
instance serialize [a] | serialize a where
    write [n : ns] c       = ["NonEmpty" : (write n (write ns c))]
    write [] c             = ["Empty" : c]
    read  ["Empty" : r]    = Just ([], r)
    read  ["NonEmpty" : r] = case read r of
                                 Nothing -> Nothing
                                 Just (x, xs) -> case read xs of
                                                     Nothing -> Nothing
                                                     Just (x`, xs`) -> Just ([x : x`], xs`)
    read  _                = Nothing
    
:: Bin a = Leaf | Bin (Bin a) a (Bin a)

instance serialize (Bin a) | serialize a where
    write Leaf c        = ["Leaf" : c]
    write (Bin l m r) c = ["Bin" : write l (write m (write r c))]
    read ["Leaf" : r]   = Just (Leaf, r)
    read ["Bin"  : r]   = case read r of 
                              Nothing -> Nothing 
                              Just (left, rest) -> case read rest of
                                                   Nothing -> Nothing 
                                                   Just (mid, rest`) -> case read rest` of
                                                                            Nothing -> Nothing 
                                                                            Just (right, rest``) -> Just ((Bin left mid right), rest``)
    read _              = Nothing
                                                                             
:: Rose a = Rose a [Rose a]

instance serialize (Rose a) | serialize a where 
    write (Rose l r) c = ["Rose" : write l (write r c)]
    read ["Rose" : r] = case read r of
                            Nothing -> Nothing
                            Just (left, rest) -> case read rest of
                                                    Nothing -> Nothing
                                                    Just (right, rest`) -> Just ((Rose left right), rest`)
    read _             = Nothing 
    
// Define equality for (Bin a) in order to test it
    
instance == (Bin a) | == a where
    == Leaf Leaf = True
    == (Bin left1 mid1 right1) (Bin left2 mid2 right2) = left1 == left2 && mid1 == mid2 && right1 == right2
    == _ _ = False
    
// Define equality for (Rose a) in order to test it

instance == (Rose a) | == a where 
    == (Rose left1 right1) (Rose left2 right2) = left1 == left2 && right1 == right2
    == _ _ = False
    
test :: a -> (Bool, [String]) | serialize, ==a
test a = (isJust r && fst jr ==a && isEmpty (tl (snd jr)), s)
where
    s = write a ["\n"]
    r = read s
    jr = fromJust r

//Start = map test [True, False]
//Start = map test [1, 2, 3]
//Start = map test [[1, 2, 3]]
//Start = map test [Bin Leaf 77 (Bin Leaf 5 Leaf)]
Start = map test [Rose 777 [Rose 77 [Rose 7 []]]]
