// Brigel Pineti s1005549
// Tim lastName studentID

module typeConstructorClasses

import StdEnv

class Container t where
    Cinsert   :: a (t a) -> t a      | <        a
    Ccontains :: a (t a) -> Bool     | <, Eq    a
    Cshow     ::   (t a) -> [String] | toString a
    Cnew      :: t a
    
Start = "False" 

