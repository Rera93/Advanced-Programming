// Brigel Pineti s1005549
// Tim Turksema s1013838

module genericMap

/*
  Genric map definition for assignment 3 in AFP 2018
  Pieter Koopman, pieter@cs.ru.nl
  September 2018
  
  Use StdEnv or iTask environment.
*/

import StdEnv, StdGeneric, GenEq

generic gMap a b :: a -> b
gMap{|Int|}         x = x
gMap{|Real|}        x = x
gMap{|UNIT|}        x = x
gMap{|PAIR|}   f g (PAIR x y) = PAIR   (f x) (g y) 
gMap{|EITHER|} f g (LEFT x)   = LEFT   (f x)
gMap{|EITHER|} f g (RIGHT x)  = RIGHT  (g x)
gMap{|CONS|}   f   (CONS x)   = CONS   (f x)
gMap{|OBJECT|} f   (OBJECT x) = OBJECT (f x)


:: Bin a = Leaf | Bin (Bin a) a (Bin a)
derive gMap [], Bin, (,)
t = Bin (Bin Leaf 1 Leaf) 2 (Bin (Bin Leaf 3 Leaf) 4 Leaf)
l = [1..7]

fac :: Int -> Int
fac 0 = 1
fac n = n * fac (n-1)

Start = gEq{|* -> *|} (<) [1,2] [2,3]

//3.1.1
//gMap{|*->*|} fac t
//3.1.2
//gMap{|*->*|} (\x -> (x, fac x)) l
//3.1.3
//gMap{|*->*->*|} (gMap{|*->*|} fac) (gMap{|*->*|} fac) (l,t)	

//3.2
// For this part, you first have to append a library to your project paths. 
// When running this, the IDE will ask for the path to the GenEq library, since it cannot find it in the Project Paths.
// This library is under Libraries/Generics/GenEq.icl
// Add this library, and then this program should function as needed.
//3.2.1
//gEq{|*|} [1,2] [1,2]
//3.2.2
//gEq{|*|} [1,2] [2,3]
//3.2.3
//gEq{|* -> *|} (<) [1,2] [2,3]
