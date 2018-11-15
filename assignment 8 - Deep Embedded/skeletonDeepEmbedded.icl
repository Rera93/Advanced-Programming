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
import Data.Tuple

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

instance == Var where
  (==) s1 s2 = s1 == s2
  (==) _ _   = False
  
// 1.3

:: Sem a = Sem (State -> (Either String (a, State))

:: Fail :== String

instance == Fail where
    (==) f1 f2 = f1 == f2
    (==) _ _        == False

unSem :: (Sem a) -> State -> Either Fail (a, State)
unSem (S s) = s
  
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
