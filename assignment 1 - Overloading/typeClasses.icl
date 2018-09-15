// Brigel Pineti s1005549
// Tim lastName studentID

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
    
test :: a -> (Bool, [String]) | serialize, ==a
test a = (isJust r && fst jr ==a && isEmpty (tl (snd jr)), s)
where
    s = write a ["\n"]
    r = read s
    jr = fromJust r

Start = map test [[1, 2]]