module skeletonDeepEmbedded

/*
  Advanved Progrmming 2018, Assignment 8
  Pieter Koopman, pieter@cs.ru.nl
*/

import iTasks => qualified return, >>=, >>|, sequence, forever, :: Set
/*
	qualified import of the named objects to avoid name conflicts.
	Use this as 'iTasks'.return. All other parts of iTasks are available.
*/
import Data.Functor, Control.Applicative, Control.Monad
import Data.Tuple, Data.Either

import qualified Data.List as List
import qualified Data.Map as Map
// use this as: 'List'.union

// 1.1

:: Expression
  = New      [Int]
  | Elem     Int
  | Variable Ident
  | Size     Set
  | (+.) infixl 6 Expression Expression
  | (-.) infixl 6 Expression Expression
  | (*.) infixl 7 Expression Expression
  | (=.) infixl 2 Ident Expression

:: Set    :== Expression
:: Elem  :== Expression
:: Ident  :== String

// === State

// 1.2

:: Val = IntV Int | SetV [Int]

:: State :== 'Map'.Map Var Val
:: Var :== String

/*instance == Var where
  (==) s1 s2 = s1 == s2
  (==) _ _   = False*/
  
// 1.3

:: Sem a = Sem (State -> Either String (a, State))

:: Fail :== String

/*instance == Fail where
    (==) f1 f2 = f1 == f2
    (==) _ _   = False*/

unSem :: (Sem a) -> State -> Either Fail (a, State)
unSem (Sem s) = s

instance Functor Sem where
  //fmap :: (a->b) (Sem a) -> (Sem b)
    fmap f (Sem g) = Sem \st -> case g st of
                                    (Left m)        = Left m
                                    (Right (v, st)) = Right ((f v), st)
    
instance Applicative Sem where
  //pure :: a -> Sem a  
    pure x = Sem \st -> Right (x, st)
  //(<*>) infixl 4 :: (Sem (a->b)) (Sem a) -> Sem b
    (<*>) ff ss = ff >>= \f -> ss >>= \s -> pure (f s)
    
instance Monad Sem where
  //bind :: (m a) (a->m b) -> m b
    bind (Sem g) f = Sem \st -> case g st of
                                (Left m)        = Left m
                                (Right (v, st)) = unSem (f v) st  
  
// 2.1
 
:: Logical
  = TRUE | FALSE
  | (In) infix 4 Elem Set
  | (==.) infix 4 Expression Expression
  | (<=.) infix 4 Expression Expression
  | Not Logical
  | (||.) infixr 2 Logical Logical
  | (&&.) infixr 3 Logical Logical

:: Stmt
  = Expression Expression
  | Logical Logical
  | For Ident Set Stmt
  | If Logical Stmt Stmt 
  
  



// === semantics


// === simulation

(>>>=)     :== tbind
(>>>|) a b :== tbind a (\_ -> b)

Start = ()
