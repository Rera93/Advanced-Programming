// Brigel Pineti s1005549
// Tim Turksema s1013838

module skeletonShallowEmbedded

/*
  Advanved Progrmming 2018, Assignment 9
  Pieter Koopman, pieter@cs.ru.nl
*/

import Data.Functor, Control.Applicative, Control.Monad
import Data.Tuple, Data.Either, Data.List
import StdString
import qualified Data.List as List
import qualified Data.Map as Map
// use this as: 'List'.union

// 1. State

:: Eval a = Eval (State -> Either Fail (a, State))
:: Fail :== String

:: Sem a = {eval :: Eval a, print :: [String] -> [String]}

:: State :== 'Map'.Map Ident Dynamic
:: Ident  :== String

/* Since we have never used Dynamic before, we thought of this exercise
   as a great opportunity to try it and learn from it. 
   In addition, we believe that using the same structure for State as in
   the previous exercise might complicate things in this exercise. r*/
   
instance Functor Eval where
  //fmap :: (a->b) (Eval a) -> (Eval b)
    fmap atob (Eval g) = Eval \st -> case g st of 
                                       (Left m)        = Left m
                                       (Right (a, st)) = Right (atob a, st)

instance Applicative Eval where
  //pure :: a -> Eval a
    pure a = Eval \st -> Right (a, st)
  //<*> infixl 4 :: (Eval (a->b)) (Eval a) -> Eval b 
    <*> (Eval f) (Eval g) = Eval \st -> case f st of
                                         (Right (atob, st)) = case g st of
                                                                  (Right (a, st)) = Right (atob a, st)
                                                                  (Left m)        = Left m
                                         (Left m)           = Left m

instance Monad Eval where
  //bind :: (Eval a) (a-> Eval b) -> Eval b
    bind (Eval g) atomb = Eval \st -> case g st of 
                                        (Left m)        = Left m
                                        (Right (a, st)) = unEval (atomb a) st
                                        
unEval :: (Eval a) -> State -> Either Fail (a, State)
unEval (Eval e) = e   

// 2. Integer Expressions 

:: Element :== Sem Int
:: Set     :== Sem [Int] 

integer :: Int -> Element
integer x = {eval = pure x, print = \p -> [toString x : p]}

size :: Set -> Element
size set = {eval = fmap length set.eval, print = \p -> ["sizeOf(" : set.print [")" : p]]}

createSet :: [Int] -> Set
createSet set = {eval = pure set, print = \p -> [toString set : p]}

  

                                                 
Start = size (createSet [3, 4])
