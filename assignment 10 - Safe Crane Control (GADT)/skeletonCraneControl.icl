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
  | Wait                (BM a (Either High Low)) (BM b (Either High Low))  // do nothing
  | (:.) infixl 1       (Action a b)             (Action a b)              // sequence of two actions
  | WhileContainerBelow (Action a b)                                       /* repeat action while there is a container 
                                                                              at current pos */
  
:: High = High
:: Low  = Low

:: BM a b = { f :: a -> b, t :: b -> a } // bimap 

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
              
:: Eval a = Eval (State -> ErrorOrResult Fail (a, State))

eval :: (Eval a) -> ErrorOrResult Fail (a, State)
eval action       = unEval action initialState 

unEval :: (Eval a) -> State -> ErrorOrResult Fail (a, State)
unEval (Eval action) = action 

fail :: Fail -> Eval a 
fail m = Eval \_ -> Error m 
   
instance Functor Eval where
  //fmap :: (a->b) (Eval a) -> (Eval b)
    fmap atob (Eval g) = Eval \st -> case g st of 
                                       (Error m)        = Error m
                                       (Result (a, st)) = Result (atob a, st)

instance Applicative Eval where
  //pure :: a -> Eval a
    pure a = Eval \st -> Result (a, st)
  //<*> infixl 4 :: (Eval (a->b)) (Eval a) -> Eval b 
    <*> (Eval f) (Eval g) = Eval \st -> case f st of
                                         (Result (atob, st)) = case g st of
                                                                  (Result (a, st)) = Result (atob a, st)
                                                                  (Error m)         = Error m
                                         (Error m)           = Error m

instance Monad Eval where
  //bind :: (Eval a) (a-> Eval b) -> Eval b
    bind (Eval g) atomb = Eval \st -> case g st of 
                                        (Error m)        = Error m
                                        (Result (a, st)) = unEval (atomb a) st
/*

store :: Ident (Eval a) -> Eval a | TC a
store i (Eval e) = Eval \st -> case e st of
                                   (Left m)        = Left m 
                                   (Right (a, st)) = Right (a, 'Map'.put i (dynamic a) st)                                

read :: Ident -> Eval a | TC a
read i = Eval \st -> case 'Map'.get i st of
                                  Just (x :: a^) = Right (x, st)
                                  Just _ = Left ("The type of variable " +++ i +++ "does not match")
                                  _ = Left ("Variable " +++ i +++ " could not be found")*/
                                  
//store :: (Eval a) -> Eval a
//store 

Start = "True"
