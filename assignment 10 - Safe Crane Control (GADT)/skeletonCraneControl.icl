// Brigel Pineti s1005549
// Tim Turksema s1013838

module skeletonCraneControl

/*
  Advanved Progrmming 2018, Assignment 10
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

:: BM a b = { f :: a -> b, t :: b -> a } // bimap 

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
eval (MoveToShip bma bmb) s            = Result {State | s & craneOnQuay = False}
eval (MoveToQuay bma bmb) s            = Result {State | s & craneOnQuay = True}
eval (MoveUp     bma bmb) s            = Result {State | s & craneUp     = True}
eval (MoveDown   bma bmb) s            = Result {State | s & craneUp     = False}
eval (Lock       bma bmb) s            = Result {State | s & locked      = if (s.craneOnQuay) (Just (head s.onQuay)) (Just (head s.onShip))
                                                           , onShip      = if (s.craneOnQuay) (s.onShip) ('List'.delete (head s.onShip) s.onShip)
                                                           , onQuay      = if (s.craneOnQuay) ('List'.delete (head s.onQuay) s.onQuay) (s.onQuay)} 
eval (UnLock     bma bmb) s            = Result {State | s & locked      = Nothing
                                                           , onShip      = if (s.craneOnQuay) (s.onShip) ('List'.union [head s.onQuay] s.onShip)
                                                           , onQuay      = if (s.craneOnQuay) ('List'.union [head s.onShip] s.onQuay) (s.onQuay)}
eval (Wait       bm)      s            = Result s
eval (actionL :. actionR) s            = case eval actionR s of
                                             Result s` = eval actionL s`
                                             Error m   = Error m  
eval _ s = Error "error"
//eval (WhileContainerBelow bm action) s = 

//loadShip :: Action a b
//loadShip = 

Start = eval (Wait bm :. Lock bm bm) initialState
