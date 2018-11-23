// Brigel Pineti s1005549
// Tim Turksema s1013838

module skeletonShallowEmbedded

/*
  Advanved Progrmming 2018, Assignment 9
  Pieter Koopman, pieter@cs.ru.nl
*/

import Data.Functor, Control.Applicative, Control.Monad
import Data.Tuple, Data.Either
import GenPrint

import qualified Data.List as List
import qualified Data.Map as Map
// use this as: 'List'.union

// 1. State

:: Sem a = Sem (State -> Either Fail (a, State))
:: Fail :== String

:: Views a = {eval :: Sem a, print :: [String] -> [String]}

:: State :== 'Map'.Map Ident Dynamic
:: Ident  :== String

/* Since we have never used Dynamic before, we thought of this exercise
   as a great opportunity to try it and learn from it. 
   In addition, we believe that using the same structure for State as in
   the previous exercise might complicate things in this exercise. r*/
   
instance Functor Sem where
  //fmap :: (a->b) (Sem a) -> (Sem b)
    fmap atob (Sem g) = Sem \st -> case g st of 
                                       (Left m)        = Left m
                                       (Right (a, st)) = Right (atob a, st)

instance Applicative Sem where
  //pure :: a -> Sem a
    pure a = Sem \st -> Right (a, st)
  //<*> infixl 4 :: (Sem (a->b)) (Sem a) -> Sem b 
    <*> (Sem f) (Sem g) = Sem \st -> case f st of
                                         (Right (atob, st)) = case g st of
                                                                  (Right (a, st)) = Right (atob a, st)
                                                                  (Left m)        = Left m
                                         (Left m)           = Left m

instance Monad Sem where
  //bind :: (Sem a) (a-> Sem b) -> Sem b
    bind (Sem g) atomb = Sem \st -> case g st of 
                                        (Left m)        = Left m
                                        (Right (a, st)) = unSem (atomb a) st
                                        
unSem :: (Sem a) -> State -> Either Fail (a, State)
unSem (Sem s) = s   
  

                                                 
Start = "eval (True)"
