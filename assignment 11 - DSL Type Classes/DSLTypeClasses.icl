// Brigel Pineti s1005549
// Tim Turksema s1013838

module DSLTypeClasses

/*
  Advanved Programming 2018, Assignment 10
  Pieter Koopman, pieter@cs.ru.nl
*/
from Data.Func import $
import Data.Tree
import StdMaybe
import Data.Functor, Control.Applicative, Control.Monad
import Data.Tuple, Data.Either, Data.List
import StdString
import StdDynamic
import StdEnv
import StdBool
import qualified Data.List as List
import qualified Data.Map as Map
import Text => qualified join
// use this as: 'List'.union

// 1. Adding Safety 

// a => initial position of crane
// b => final position of crane

/*
:: Action
 = MoveToShip // move the crane to the ship
 | MoveToQuay // move the crane to the quay
 | MoveUp // moves the crane from down to up position
 | MoveDown // moves the crane from up to down position
 | Lock // locks the top container on the stack the crane is currently above
 | Unlock // unlocks the container the crane is carrying, put it on the stack
 | Wait // do nothing
 | (:.) infixl 1 Action Action // sequence of two actions
 | While (Expr Bool) Action // repeat action while expression yields true

:: Expr x
 = ContainersBelow :: (Expr Int) // number of containers at current position
 | Lit :: t -> Expr t | toString t
 | (<.) infix 4 :: (Expr t) (Expr t) -> Expr Bool | <, toString t
 | (>.) infix 4 :: (Expr t) (Expr t) -> Expr Bool | <, toString t
 | (+.) infix 4 :: (Expr Int) (Expr Int) -> Expr Int
  */                                                          
:: High = High
:: Low  = Low

:: Step pre post = Step


class Action v where
	MoveToShip :: v (Step High High)
	MoveToQuay :: v (Step High High)
	MoveUp     :: v (Step Low High)
	MoveDown   :: v (Step High Low)
	Lock       :: v (Step Low Low)
 	Unlock     :: v (Step Low Low)
 	Wait          :: v (Step a a)
    (:.) infixl 1 :: (v (Step a b)) (v (Step b c)) -> v (Step a c)
	While 	      :: (v Bool) (v (Step a a)) -> v (Step a a)

class Expr v where
	ContainersBelow :: v Int
	Lit             :: t -> v t                 | toString t
	(<.) infix 4    :: (v t) (v t) -> v Bool    | Ord t
	(>.) infix 4    :: (v t) (v t) -> v Bool    | Ord t
	(+.) infix 4    :: (v t) (v t) -> v t | + t
	

:: Show a = S ([String] -> [String])
runShow (S a) = a

instance Expr Show where
	ContainersBelow = S \c -> ["containersBelow":c]
	Lit t = S \c -> [toString t:c]
	(+.) (S a) (S b) = S \c -> a ["+":b c]
    (<.) (S a) (S b) = S \c -> a ["<":b c]
    (>.) (S a) (S b) = S \c -> a [">":b c]
    
instance Action Show where
	(:.)  (S a) (S b) = S \c -> a [":. \n\t":b c]
	MoveToShip        = S \c -> ["MoveToShip":c]
	MoveToQuay        = S \c -> ["MoveToQuay":c]
	MoveUp            = S \c -> ["MoveUp":c]
	MoveDown          = S \c -> ["MoveDown":c]
	Lock              = S \c -> ["Lock":c]
	Unlock            = S \c -> ["Unlock":c]
	Wait              = S \c -> ["Wait":c]
	While (S a) (S b) = S \c -> ["While(":(a [])] ++ ["){":(b ["}":c])]
	

test = While 
			(ContainersBelow >. Lit 0) 
			{MoveUp}
Start = runShow test []



