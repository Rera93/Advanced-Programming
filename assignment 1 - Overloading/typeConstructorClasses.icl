// Brigel Pineti s1005549
// Tim Turksema s1013838

module typeConstructorClasses

import StdEnv

class Container t where
    Cinsert   :: a (t a) -> t a      | <        a
    Ccontains :: a (t a) -> Bool     | <, Eq    a
    Cshow     ::   (t a) -> [String] | toString a
    Cnew      :: t a
    
instance Container [] where
    Cinsert i []     = [i]
    Cinsert i list   = [i : list]
    Ccontains i []   = False
    Ccontains i list = isMember i list
    Cshow []         = []
    Cshow list       = map toString list
    Cnew             = []
    
:: Bin a = Leaf | Bin (Bin a) a (Bin a)
    
instance Container Bin where
    Cinsert i Leaf          = Bin Leaf i Leaf
    Cinsert i (Bin l m r)
      | i > m               = Bin l m (Cinsert i r)
      | otherwise           = Bin (Cinsert i l) m r  
    Ccontains i (Bin l m r)
      | i == m              = True
      | i < m               = Ccontains i l 
      | otherwise           = Ccontains i r
    Ccontains _ Leaf        = False
    Cshow (Bin l m r)       = Cshow l ++ [toString m : Cshow r]  
    Cshow Leaf              = []
    Cnew                    = Leaf
    
// Define equality for (Bin a) in order to test it
    
instance == (Bin a) | == a where
    == Leaf Leaf = True
    == (Bin left1 mid1 right1) (Bin left2 mid2 right2) = left1 == left2 && mid1 == mid2 && right1 == right2
    == _ _ = False

//Start = (Ccontains 3 c, Cshow c, Cinsert 0 c) where c = [1..5]
Start = (Ccontains 9 c, Cshow c, Cinsert 7 c) 
    where c = (Bin (Bin (Bin Leaf 1 Leaf) 2 Leaf) 4 (Bin Leaf 6 (Bin Leaf 8 Leaf)))
