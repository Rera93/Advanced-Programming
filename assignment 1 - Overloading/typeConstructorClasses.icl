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
    
Start = (Ccontains 3 c, Cshow c, Cinsert 0 c) where c = [1..5] 

