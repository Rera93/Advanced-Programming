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
  = MoveToShip          (BM a High)              (BM b High)               // move crane to the ship 
  | MoveToQuay          (BM a High)              (BM b High)               // move crane to the quay
  | MoveUp              (BM a Low)               (BM b High)               // move crane from down to up position
  | MoveDown            (BM a High)              (BM b Low)                // move crane from top to down position
  | Lock                (BM a Low)               (BM b Low)                // lock the top container on the stack 
  | UnLock              (BM a Low)               (BM b Low)                // unlocks the container (put it in stack) 
  | Wait                (BM a b)                                           // do nothing
  | E.i: (:.) infixl 1  (Action a i)             (Action i b)              // sequence of two actions
  | WhileContainerBelow (BM a b)                 (Action a b)              /* repeat action while there is a container 
                                                                              at current pos */ 
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

eval :: (Action a b) State -> ErrorOrResult Fail State   
eval (MoveToShip bma bmb) s = Result ({State | s & craneOnQuay = False})
eval (MoveToQuay bma bmb) s = Result ({State | s & craneOnQuay = True})
eval (MoveUp bma bmb) s = Result ({State | s & craneUp = True})
eval (MoveDown bma bmb) s = Result ({State | s & craneUp = False})
eval (Lock bma bmb) s = case s.locked of 
							Just a = Error "The crane is already locked!"
							Nothing = case s.craneOnQuay of
											True = case s.onQuay of
													[x:xs] = Result ({State | s & onQuay = xs, locked = Just x })
													[]     = Result s
											False = case s.onShip of
													[x:xs] = Result ({State | s & onShip = xs, locked = Just x })
													[]     = Result s
eval (UnLock bma bmb) s = case s.locked of 
							Nothing = Error "The crane is already un-locked!"
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
										(Error m)  =  Error m
eval (WhileContainerBelow bma action) s = case action of
                                                 (actionL :. actionR) = eval (actionL :. actionR) s
                                                 _ = Result s
                                                /* case s.craneOnQuay of
										True = case s.onQuay of
													[] = Result s
													xs = case eval action s of 
																Result s` = eval (WhileContainerBelow bma action) s`
										_    = case s.onShip of
													[] = Result s
													xs = case eval action s of
																Result s` = eval (WhileContainerBelow bma action) s`*/

// 3. Printing 
                                                 
class printing a :: a [String] -> [String]

instance printing (Action a b) where
      printing (MoveToShip bma bmb) c            = c ++ ["        MoveToShip"]
      printing (MoveToQuay bma bmb) c            = c ++ ["        MoveToQuay"]
      printing (MoveUp     bma bmb) c            = c ++ ["        MoveUp"]
      printing (MoveDown   bma bmb) c            = c ++ ["        MoveDown"]
      printing (Lock       bma bmb) c            = c ++ ["        Lock"]
      printing (UnLock     bma bmb) c            = c ++ ["        UnLock"]
      printing (Wait       bm)      c            = c ++ ["        Wait"]
      printing (actionL :. actionR) c            = [(foldr (\x acc -> x +++ acc) "" (printing actionL c)) +++ ":."] ++ ["\n"] ++ (printing actionR c)
      printing (WhileContainerBelow bm action) c = c ++ [(unlines (["\nWhileContainerBelow \n    ("] ++ (printing action c) ++ ["    )"]))]
      
// Problem with printing (2 extra new lines applied to the last action)

moveFromQuayToShip = ( MoveDown bm bm :. 
                       Lock bm bm :.
                       MoveUp bm bm :. 
                       MoveToShip bm bm :. 
                       Wait bm :. 
                       MoveDown bm bm :. 
                       Wait bm :. 
                       UnLock bm bm :. 
                       MoveUp bm bm :. 
                       MoveToQuay bm bm
                     )
                     
// Passed Test Cases

Start = printing (WhileContainerBelow bm  (moveFromQuayToShip)) []
//Start = eval (WhileContainerBelow bm  (moveFromQuayToShip)) initialState
//Start = eval (WhileContainerBelow bm (Lock bm bm :. Lock bm bm)) initialState
//Start = eval (WhileContainerBelow bm (UnLock bm bm :. MoveUp bm bm :. MoveDown bm bm :. UnLock bm bm)) initialState

// Failed Test Case

//Start = eval (WhileContainerBelow bm (MoveDown bm bm :. Lock bm bm :. MoveUp bm bm :. MoveToShip bm bm :. MoveDown bm bm)) initialState