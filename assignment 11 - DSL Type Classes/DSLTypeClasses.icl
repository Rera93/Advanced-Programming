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

// 1. DSL 
                                                         
:: High = High
:: Low  = Low

:: Step pre post = Step

class Action v where
	MoveToShip :: v (Step High High)
	MoveToQuay :: v (Step High High)
	MoveUp     :: v (Step Low High)
	MoveDown   :: v (Step High Low)
	Lock       :: v (Step Low Low)
 	UnLock     :: v (Step Low Low)
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

instance Action Show where
  MoveToShip        = S \c -> ["MoveToShip" : c]
  MoveToQuay        = S \c -> ["MoveToQuay" : c]
  MoveUp            = S \c -> ["MoveUp" : c]
  MoveDown          = S \c -> ["MoveDown" : c]
  Lock              = S \c -> ["Lock" : c]
  UnLock            = S \c -> ["UnLock" : c]
  Wait              = S \c -> ["Wait" : c]
  (:.) (S a) (S b)  = S \c -> a [":.\n    " : b c]
  While (S e) (S a) = S \c -> ["While ( " : e c] ++ [" ) (\n    " : a c] ++ ["\n  )"]
  
instance Expr Show where
  ContainersBelow  = S \c -> ["ContainersBelow" : c]
  Lit t            = S \c -> [toString t : c]
  (<.) (S a) (S b) = S \c -> a [" < " : b c]
  (>.) (S a) (S b) = S \c -> a [" > " : b c]
  (+.) (S a) (S b) = S \c -> a [" + " : b c]


loadShip =
    While (ContainersBelow >. Lit 0) (
        MoveDown:.
        Lock:.
        MoveUp:.
        MoveToShip:.
        Wait:.
        MoveDown:.
        Wait:.
        UnLock:.
        MoveUp:. 
        MoveToQuay
       )
	

Start = concat (runShow loadShip [])



