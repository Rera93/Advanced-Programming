// Brigel Pineti s1005549
// Tim Turksema s1013838

module skeletonShallowEmbedded

/*
  Advanved Progrmming 2018, Assignment 9
  Pieter Koopman, pieter@cs.ru.nl
*/
from Data.Func import $ 
import Data.Functor, Control.Applicative, Control.Monad
import Data.Tuple, Data.Either, Data.List
import StdString
import StdDynamic
import StdBool
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

// 2. Integer & 3. Set Expressions 

:: Element :== Sem Int
:: Set     :== Sem [Int] 

eval :: (Sem a) -> Either Fail (a, State)
eval sem = unEval sem.eval 'Map'.newMap

store :: Ident (Eval a) -> Eval a | TC a
store i (Eval e) = Eval \st -> case e st of
                                   (Left m)        = Left m 
                                   (Right (a, st)) = Right (a, 'Map'.put i (dynamic a) st)
                                   
                                   
/*read :: Ident -> Eval a | TC a
read i = Eval \st -> case 'Map'.get i st of
                         unpack (x :: a^) = Right (x , st)
                         unpack _ = Left ("The type of variable " +++ i +++ "does not match") 
                         _ = Left ("Variable " +++ i +++ " could not be found")*/

integer :: Int -> Element
integer x = {eval = pure x, print = \p -> [toString x : p]}

size :: Set -> Element
size set = {eval = length <$> set.eval, print = \p -> ["sizeOf(" : set.print [")" : p]]}

newSet :: [Int] -> Set
newSet set = {eval = pure set, print = \p -> [toString set : p]}

//var :: Ident - Sem a | TC a
//var i = {eval = read i, print = \p -> [i : p] } 

instance + (Sem a) | + a where
    (+) el er = {eval = (+) <$> el.eval <*> er.eval, print = \p -> el.print ["+" : er.print p]}
    
instance - (Sem a) | - a where
    (-) el er = {eval = (-) <$> el.eval <*> er.eval, print = \p -> el.print ["+" : er.print p]}
    
instance * (Sem a) | * a where
    (*) el er = {eval = (*) <$> el.eval <*> er.eval, print = \p -> el.print ["*" : er.print p]} 
    
(=.) infixl 2 :: Ident (Sem a) -> Sem a | TC a      // Ident =. Dynamic 
(=.) i sem = {eval = store i sem.eval, print = \p -> [i, "=" : sem.print p]}

(+=) infixl 4 :: Element Set -> Set                 // Int +. [Int] 
(+=) elem set = { eval = (\e s -> 'List'.union [e] s) <$> elem.eval <*> set.eval
                , print = \p -> elem.print ["+" : set.print p]
                }    
                
(=+) infixl 4 :: Set Element -> Set                // [Int] +. Int
(=+) set elem = { eval = (\s e -> 'List'.union s [e]) <$> set.eval <*> elem.eval
                , print = \p -> set.print ["+" : elem.print p]
                }
                
(=-) infixl 4 :: Set Element -> Set               // [Int] -. Int   (delete Int from Set)
(=-) set elem = { eval = (\s e -> 'List'.difference s [e]) <$> set.eval <*> elem.eval
                , print = \p -> set.print ["-" : elem.print p]
                }
                
(=*) infixl 4 :: Set Element -> Set               // [Int] *. Int 
(=*) set elem = { eval = (\s e -> 'List'.intersect s [e]) <$> set.eval <*> elem.eval
                , print = \p -> set.print ["*" : elem.print p]
                }
               
instance + [a] | == a where
    (+) ll lr = 'List'.union ll lr

instance - [a] | == a where
    (-) ll lr = 'List'.difference ll lr
    
instance * [a] | == a where
    (*) ll lr = 'List'.intersect ll lr
                
// Boolean Expressions

(In) infixl 4 :: Element Set -> Sem Bool
(In) elem set = { eval = isMember <$> elem.eval <*> set.eval
                , print = \p -> elem.print ["In" : set.print p]
                } 
(==.) infix 4 :: (Sem a) (Sem a) -> Sem Bool | == a
(==.) semL semR = { eval = (==) <$> semL.eval <*> semR.eval
                  , print = \p -> semL.print ["==" : semR.print p]
                  } 
                  
(<=.) infixl 4 :: Element Element -> Sem Bool
(<=.) elemL elemR = { eval = (<=) <$> elemL.eval <*> elemR.eval
                    , print = \p -> elemL.print ["<=" : elemR.print p]
                    }
                    
(||.) infixl 4 :: (Sem Bool) (Sem Bool) -> Sem Bool
(||.) semL semR = { eval = (||) <$> semL.eval <*> semR.eval
                  , print = \p -> semL.print ["||" : semR.print p]
                  }

(&&.) infixl 4 :: (Sem Bool) (Sem Bool) -> Sem Bool
(&&.) semL semR = { eval = (&&) <$> semL.eval <*> semR.eval
                  , print = \p -> semL.print ["&&" : semR.print p]
                  }

(!.) :: (Sem Bool) -> Sem Bool
(!.) sem = { eval = not <$> sem.eval
           , print = \p -> ["!" : sem.print p]
           }
          
true :: Sem Bool
true = { eval = pure True, print = \p -> ["True" : p]}

false :: Sem Bool
false = { eval = pure False, print = \p -> ["False" : p]}

// 4. Statements


                                                 
//Start = eval (integer 2 + integer 3)
//Start = size (createSet [3, 4])
//Start = eval ("a" =. (integer 2 += newSet [3, 4]))
//Start = eval ((true &&. false) ||. false)
//Start = eval ((newSet [2, 3]) ==. (newSet [2, 4]))
Start = eval (newSet [2, 3, 4] - newSet [2, 3])