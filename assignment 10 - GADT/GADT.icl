// Brigel Pineti s1005549
// Tim Turksema s1013838

module GADT

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


:: Action a b
  = MoveToShip          (BM a High)              (BM a High)               // move crane to the ship 
  | MoveToQuay          (BM a High)              (BM a High) // move crane to the quay
  | MoveUp              (BM a Low)               (BM b High)               // move crane from down to up position
  | MoveDown            (BM a High)              (BM b Low)                // move crane from top to down position
  | Lock                (BM a Low)               (BM b Low)                // lock the top container on the stack 
  | UnLock              (BM a Low)               (BM b Low)                // unlocks the container (put it in stack) 
  | Wait                (BM a b)  // do nothing
  | E.i: (:.) infixl 1  (Action a i)             (Action i b)              // sequence of two actions
  | WhileContainerBelow (Action a b) 			 (BM a b)                                       /* repeat action while there is a container at current pos */
 
 // a a 
:: High = High
:: Low  = Low

/*:: Expr a
	= Lit a
	| E. e: Plus (BM a e) (Expr e) (Expr e) & + e
	
:: Phantom a = P Int
	
evalExpr :: (Expr a) -> Maybe a
evalExpr (Lit a) = Just a
evalExpr (Plus bm l r) = bm.t (evalExpr l + evalExpr r)
*/
//Start = evalExpr (Plus bm (Lit 41) (Lit 1))

:: BM a b = { f :: a -> b, t :: b -> a} // bimap 

bm :: BM a a
bm = { f = id, t = id }

// 2. Evaluation

:: ErrorOrResult e r = Error e | Result r

// Error to be produced when an action cannot be executed in the current state.

:: State = { onShip      :: [Container]
           , onQuay      :: [Container]
           , craneUp     :: Bool
           , craneOnQuay :: Bool
           , locked      :: Maybe Container
           }

:: Container :== String

initialState :: State
initialState = { onShip      = []
               , onQuay      = ["apples", "beer", "camera's"]
               , craneUp     = True
               , craneOnQuay = True
               , locked      = Nothing
               } 
              
:: Fail :== String 

// do we return Action ?? (Eval (Action a b))

eval :: (Action a b) State -> ErrorOrResult Fail State   
eval (MoveToShip bma bmb) s = Result ({State | s & craneOnQuay = False})
eval (MoveToQuay bma bmb) s = Result ({State | s & craneOnQuay = True})
eval (MoveUp bma bmb) s = Result ({State | s & craneUp = True})
eval (MoveDown bma bmb) s = Result ({State | s & craneUp = False})
eval (Lock bma bmb) s = case s.locked of 
							Just a = Result s
							Nothing = case s.craneOnQuay of
											True = case s.onQuay of
													[x:xs] = Result ({State | s & onQuay = xs, locked = Just x })
													[]     = Result s
											False = case s.onShip of
													[x:xs] = Result ({State | s & onShip = xs, locked = Just x })
													[]     = Result s
eval (UnLock bma bmb) s = case s.locked of 
							Nothing = Result s
							Just a = case s.craneOnQuay of
											True = case s.onQuay of 
														[] = Result ({State | s & onQuay = [a], locked = Nothing })
														xs = Result ({State | s & onQuay = [a:xs], locked = Nothing })
											False = case s.onShip of
														[] = Result ({State | s & onShip = [a], locked = Nothing })
														xs = Result ({State | s & onShip = [a:xs], locked = Nothing })
											
eval (Wait bma) s = Result s
eval (actionL :. actionR) s = case eval actionL s of
										(Result r) =  eval actionR r
										_ 		   =  Error "Error"
eval (WhileContainerBelow (action) (bma)) s = case s.craneOnQuay of
										True = case s.onQuay of
													[] = Result s
													xs = case eval action s of 
																Result s` = eval (WhileContainerBelow action bma) s`
										_    = case s.onShip of
													[] = Result s
													xs = case eval action s of
																Result s` = eval (WhileContainerBelow action bma) s`
eval _ _ = Error "Fail" 
/* eval (WhileContainerBelow action) s

eval action       = unEval action initialState 
*/
moveFromQuayToShip = (MoveDown bm bm :. Lock bm bm :. MoveUp bm bm :. MoveToShip bm bm :. MoveDown bm bm :. UnLock bm bm :. MoveUp bm bm :. MoveToQuay bm bm)
//test = eval moveFromQuayToShip initialState
test = eval (WhileContainerBelow (moveFromQuayToShip) bm) initialState

Start = test