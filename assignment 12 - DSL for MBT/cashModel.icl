implementation module cashModel
/*
	Pieter Koopman, Radboud University, 2017 - 2018
	pieter@cs.ru.nl
	Advanced programming
	
	A simple state model for an automated cash register
*/

import StdEnv, GenEq

class euro a :: a -> Euro
instance euro Product where
	euro Pizza = euro (4,99)
	euro Beer  = euro (0,65)
	euro _     = euro 1

instance euro Int where euro e = {euro = e, cent = 0}
instance euro (Int, Int) where euro (e,c) = {euro = e, cent = c}
instance euro [e] | euro e where euro l = sum (map euro l)
instance euro Euro where euro e = e
instance + Euro where
	+ x y = {euro = c / 100, cent = (abs c) rem 100} where
		c = (x.euro + y.euro) * 100 + sign x.euro * x.cent + sign y.euro * y.cent
instance - Euro where
	- x y = {euro = c / 100, cent = (abs c) rem 100} where
		c = (x.euro - y.euro) * 100 + sign x.euro * x.cent - sign y.euro * y.cent
instance zero Euro where zero = {euro = 0, cent = 0}
derive gEq Euro,  Product
instance == Product where (==) p q = p === q
instance == Euro where (==) p q = p === q

instance ~ Euro where ~ e = {e & euro = ~e.euro}

model :: [Product] Action -> ([Product],[Euro])
model list (Add p) = ([p:list],[euro p])
model list (Rem p) | isMember p list
 = (removeMember p list,[~ (euro p)])
 = (list,[])
model list Pay = ([],[euro list])
