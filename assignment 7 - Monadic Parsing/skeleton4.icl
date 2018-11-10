module skeleton4

// Brigel Pineti s1005549
// Tim Turksema s1013838

import qualified Data.Map as Map

import StdEnv, StdMaybe

:: Gram = Lit String
        | Idn
        | Int
        | Seq [Gram]
        | Alt [Gram]
        | Def Name Gram Gram
        | Var Name
        
:: Name :== String
 
listIntGram :: Gram
listIntGram = Def "list" (Alt [Lit "[]", Seq [Lit "[", Int, Lit ":", Var "list", Lit "]"]]) (Seq [Idn, Lit "=", Var "list"])

:: State = { input :: [String]
           , seen  :: [String]
           , store :: Store
           }
           
:: Store :== 'Map'.Map Name Gram

:: Parse a = Parse (State -> (Maybe a, State))

class MyFunctor f where
	fmap :: (a->b) (f a) -> (f b)
	(<$>) infixl 4 :: (a->b) (f a) -> (f b) | MyFunctor f
 	(<$>) f x :== fmap f x
 	
instance MyFunctor (Parse) where
    fmap f (Parse g) = Parse \st -> case g st of
                                       (Just a, st`) = (Just (f a), st`) 
                                       (_, st`) = (Nothing, st`) 

class Applicative f | MyFunctor f where
	pure :: a -> f a
	(<*>) infixl 4 :: (f (a->b)) (f a) -> f b
	
instance Applicative (Parse) where
    pure x = Parse \st -> (Just x, st)
    (<*>) (Parse f) (Parse g) = Parse \st -> case f st of
                                                 (Just f, st) = case g st of 
                                                                    (Just a, st)  = (Just (f a),st)
                                                                    (Nothing, st) = (Nothing, st)
                                                 (Nothing, st) = (Nothing, st)

class Monad m | Applicative m where
	bind :: (m a) (a->m b) -> m b
	(>>=) infixl 1 :: (m a) (a->m b) -> m b | Monad m
	(>>=) a f :== bind a f
	(>>|) infixl 1 :: (m a)    (m b) -> m b | Monad m
	(>>|) a b :== a >>= \_.b
	rtrn :: a -> m a | Monad m
	rtrn a :== pure a
	
instance Monad (Parse) where
    bind (Parse g) f = Parse \st -> case g st of 
                                        (Just a, st)  = unParse (f a) st
                                        (Nothing, st) = (Nothing, st) 
                                        
    
unParse :: (Parse a) -> State -> (Maybe a,State)
unParse (Parse f) = f   

class OrMonad m where
	(<|>) infixl 0 :: (m a) (m a) -> m a
	
instance OrMonad (Parse) where
    (<|>) (Parse f) (Parse g) = Parse \st -> case f st of
                                                (Nothing, _) = g st
                                                other        = other  


Start = "True"

