// Brigel Pineti s1005549
// Tim Turksema s1013838

module DSLTypeClasses

/*
  Advanved Programming 2018, Assignment 11
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

:: Var = Var Int

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
	Lit             :: t -> v t               | toString t
	(<.) infix 4    :: (v t) (v t) -> v Bool  | Ord t
	(>.) infix 4    :: (v t) (v t) -> v Bool  | Ord t
	(+.) infix 4    :: (v t) (v t) -> v t     | + t

class Var v where // need feedback
    (=.) infixr 2 :: (v Var) (v Int) -> v (Step a a) 
    var           :: (v Var) -> v Int
    int           :: (v Int) ((v Var) -> v (Step a a)) -> v (Step a a) 


// 2. Show

:: Show a = S (Int [String] -> [String])
runShow (S a) = a

instance Action Show where
  MoveToShip        = S \_ c -> ["MoveToShip" : c]
  MoveToQuay        = S \_ c -> ["MoveToQuay" : c]
  MoveUp            = S \_ c -> ["MoveUp" : c]
  MoveDown          = S \_ c -> ["MoveDown" : c]
  Lock              = S \_ c -> ["Lock" : c]
  UnLock            = S \_ c -> ["UnLock" : c]
  Wait              = S \_ c -> ["Wait" : c]
  (:.) (S a) (S b)  = S \i c -> a i [":.\n    " : b i c]
  While (S e) (S a) = S \i c -> ["While ( " : e i [") (\n": a i ["\n )":c]]]
  
instance Expr Show where
  ContainersBelow  = S \_ c -> ["ContainersBelow" : c]
  Lit t            = S \_ c -> [toString t : c]
  (<.) (S a) (S b) = S \i c -> a i [" < " : b i c]
  (>.) (S a) (S b) = S \i c -> a i [" > " : b i c]
  (+.) (S a) (S b) = S \i c -> a i [" + " : b i c]
  
instance Var Show where 
  (=.) (S v) (S d) = S \i c -> v i [" = " : d i c]
  var (S va)       = S va  
  int (S l) f      = S \i c -> ["Int " : l i [" \\v",toString i,".\n":runShow (f (S \_ c -> ["v", toString i:c])) (i + 1) c]]
  
// 3. Evaluation

:: State = { onShip      :: [Container]
           , onQuay      :: [Container]
           , craneUp     :: Bool
           , craneOnQuay :: Bool
           , locked      :: Maybe Container
           , store       :: 'Map'.Map Int Int
           , freshVar    :: Int
           }

:: Container :== String

initialState :: State
initialState = { onShip      = []
               , onQuay      = ["apples", "beer", "camera's"]
               , craneUp     = True
               , craneOnQuay = True
               , locked      = Nothing
               , store       = 'Map'.newMap
               , freshVar    = 0
               }
               

:: ErrorOrResult m r = Error m | Result r
:: Fail :== String

:: Evaluator a = E (State -> ErrorOrResult Fail (a, State)) 
runEval (E a) = a

instance Expr Evaluator where
  ContainersBelow  = E \s -> Result (length s.onQuay, s)
  Lit a            = pure a
  (<.) a b         = (<) <$> a <*> b // alternative approach to remove pure 
  (>.) (E a) (E b) = pure (>) <*> (E a) <*> (E b)
  (+.) (E a) (E b) = pure (+) <*> (E a) <*> (E b) 
  
instance Var Evaluator where
  var v    = v >>= \vVal -> read vVal                      
  (=.) v i = v >>= \vVal -> i >>= \iVal -> write vVal iVal
  int c f  = c >>= \conts -> E \s -> runEval (f (pure (Var s.freshVar))) {s & freshVar = inc s.freshVar, store = 'Map'.put s.freshVar conts s.store}

read :: Var -> Evaluator Int
read (Var i) = E \s -> case 'Map'.get i s.store of
                           Just b  = Result (b, s)
                           Nothing = Error "Variable not found!"
                           
write :: Var Int -> Evaluator (Step a a)
write (Var i) val =  E \s -> Result (Step, {s & store = 'Map'.put i val s.store}) 
                       
instance Action Evaluator where
  MoveToShip   = E \s -> Result (Step, {State | s & craneOnQuay = False})  
  MoveToQuay   = E \s -> Result (Step, {State | s & craneOnQuay = True})
  MoveUp       = E \s -> Result (Step, {State | s & craneUp     = True})
  MoveDown     = E \s -> Result (Step, {State | s & craneUp     = False})
  Lock         = E \s -> case s.locked of 
                                      Just a  = Error "The crane is already locked"
                                      Nothing = case s.craneOnQuay of
                                                    True  = case s.onQuay of
						                                        [x:xs] = Result (Step, {State | s & onQuay = xs, locked = Just x })
										                        []     = Result (Step, s)
						                            False = case s.onShip of
										                        [x:xs] = Result (Step, {State | s & onShip = xs, locked = Just x })
										                        []     = Result (Step, s)
  UnLock      = E \s -> case s.locked of 
                                      Nothing = Error "The crane is already un-locked"
                                      Just a  = case s.craneOnQuay of
                                                    True  = case s.onQuay of 
											                    [] = Result (Step, {State | s & onQuay = [a], locked = Nothing })
											                    xs = Result (Step, {State | s & onQuay = [a:xs], locked = Nothing })
								                    False = case s.onShip of
											                    [] = Result (Step, {State | s & onShip = [a], locked = Nothing })
											                    xs = Result (Step, {State | s & onShip = [a:xs], locked = Nothing })
  Wait        = E \s -> Result (Step, s)
  :. (E a) (E b)  = E \s -> case a s of
                                (Error m)           = Error m
                                (Result (Step, s`)) = case b s` of  
                                                        (Error m) = Error m
                                                        Result (Step, s``) = Result (Step, s``)
  While (E e) (E a) = E \s -> case e s of
                               (Result (False, _)) = Result (Step, s) //Error "Condition is false"  
                               (Result (True, _))  = case s.craneOnQuay of
                                                        True = case s.onQuay of
                                                                 []     = Result (Step, s)
                                                                 [x:xs] = case a s of
                                                                        (Error m)           = Error m
                                                                        (Result (Step, s`)) = runEval (While (E e) (E a)) s`
                                                        False = case s.onShip of
                                                                 []     = Result (Step, s)
                                                                 [x:xs] = case a s of
                                                                        (Error m)           = Error m
                                                                        (Result (Step, s`)) = runEval (While (E e) (E a)) s`
                                          

instance Functor Evaluator where
  //fmap :: (a->b) (Eval a) -> (Eval b)
    fmap atob (E g) = E \st -> case g st of 
                                       (Error m)        = Error m
                                       (Result (a, st)) = Result (atob a, st)

instance Applicative Evaluator where
  //pure :: a -> E a
    pure a = E \st -> Result (a, st)
  //<*> infixl 4 :: (E (a->b)) (E a) -> E b 
    <*> (E f) (E g) = E \st -> case f st of
                                   (Result (atob, st)) = case g st of
                                                                  (Result (a, st)) = Result (atob a, st)
                                                                  (Error m)        = Error m
                                   (Error m)           = Error m

instance Monad Evaluator where
  //bind :: (E a) (a-> E b) -> E b
    bind (E g) atomb = E \st -> case g st of 
                                        (Error m)        = Error m
                                        (Result (a, st)) = runEval (atomb a) st 

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
       
loadShip1 =
    int ContainersBelow \n.
//    int (Lit 42) \_.
    While (var n >. Lit 0) (
        MoveDown:.
        Lock:.
        MoveUp:.
        MoveToShip:.
        Wait:.
        MoveDown:.
        Wait:.
        UnLock:.
        MoveUp:. 
        MoveToQuay:.
        n =. var n +. Lit -1
       )

// Tests       
	
//Start = concat (runShow loadShip 0 [])
//Start = concat (runShow loadShip1 0 [])
//Start = runEval loadShip initialState
Start = runEval loadShip1 initialState