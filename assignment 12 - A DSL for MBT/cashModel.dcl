definition module cashModel
/*
	Pieter Koopman, Radboud University, 2017 - 2018
	pieter@cs.ru.nl
	Advanced programming
	
	A simple state model for an automated cash register
*/

import StdEnv, GenEq

:: Euro = {euro :: Int, cent :: Int}
:: Product = Pizza | Beer | Cola
:: Action = Add Product | Rem Product | Pay

class euro a :: a -> Euro
instance euro Product, Euro
instance euro Int, (Int, Int), [e] | euro e
instance + Euro
instance - Euro
instance zero Euro
derive gEq Euro
instance ~ Euro
instance == Euro, Product

model :: [Product] Action -> ([Product],[Euro])
