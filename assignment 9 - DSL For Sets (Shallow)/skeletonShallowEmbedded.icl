// Brigel Pineti s1005549
// Tim Turksema s1013838

module skeletonShallowEmbedded

/*
  Advanved Progrmming 2018, Assignment 9
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

// 1. State

:: Eval a = Eval (State -> Either Fail (a, State))
:: Fail :== String

:: Sem a = {eval :: Eval a, print :: [String] -> [String]}

:: State :== 'Map'.Map Ident Dynamic
//:: Val = IntV Int | SetV [Int] 
:: Ident  :== String

/* Since we have never used Dynamic before, we thought of this exercise
   as a great opportunity to try it and learn from it. 
   In addition, we believe that using the same structure for State as in
   the previous exercise might complicate things in this exercise. */
   
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

fail :: String -> Eval a 
fail m = Eval \_ -> Left m 

// 2. Integer & 3. Set Expressions 

:: Element :== Sem Int
:: Set     :== Sem [Int] 

eval :: (Sem a) -> Either Fail (a, State)
eval sem = unEval sem.eval 'Map'.newMap

store :: Ident (Eval a) -> Eval a | TC a
store i (Eval e) = Eval \st -> case e st of
                                   (Left m)        = Left m 
                                   (Right (a, st)) = Right (a, 'Map'.put i (dynamic a) st)                                

read :: Ident -> Eval a | TC a
read i = Eval \st -> case 'Map'.get i st of
                                  Just (x :: a^) = Right (x, st)
                                  Just _ = Left ("The type of variable " +++ i +++ "does not match")
                                  _ = Left ("Variable " +++ i +++ " could not be found")

integer :: Int -> Element
integer x = {eval = pure x, print = \p -> [toString x : p]}

size :: Set -> Element
size set = {eval = length <$> set.eval, print = \p -> ["sizeOf(" : set.print [")" : p]]}

newSet :: [Int] -> Set
newSet set = {eval = pure set, print = \p -> [printSet set : p]}

printSet :: [Int] -> String
printSet []     = "[]" 
printSet [x:xs] = "[" +++ toString x +++ " : " +++ (printSet xs) +++ "]"  

instance + Element where
    (+) el er = {eval = (+) <$> el.eval <*> er.eval, print = \p -> el.print [" + " : er.print p]}
    
instance - Element where
    (-) el er = {eval = (-) <$> el.eval <*> er.eval, print = \p -> el.print [" - " : er.print p]}
    
instance * Element where
    (*) el er = {eval = (*) <$> el.eval <*> er.eval, print = \p -> el.print [" * " : er.print p]}
    
// . symbolizes a Set
// +,-,* symbolize an Element
// E.g. The union between an Element and a Set is represented as (+.)
// E.g. The difference between a Set and an Element is represented as (.-)  

(+.) infixl 6 :: Element Set -> Set
(+.) elem set = { eval = (\e s -> 'List'.union [e] s) <$> elem.eval <*> set.eval
                     , print = \p -> elem.print [" + " : set.print p]
                     }
                     
(.+) infixl 6 :: Set Element -> Set
(.+) set elem = { eval = (\s e -> 'List'.union s [e]) <$> set.eval <*> elem.eval
                     , print = \p -> set.print [" + " : elem.print p]
                     }  
                    
(.-) infixl 6 :: Set Element -> Set
(.-) set elem = { eval = (\s e -> 'List'.difference s [e]) <$> set.eval <*> elem.eval
                     , print = \p -> set.print [" - " : elem.print p]
                     }
                    
(*.) infixl 7 :: Element Set -> Set
(*.) elem set = { eval = (\e s -> fmap ((*)e) s) <$> elem.eval <*> set.eval
                          , print = \p -> set.print [" * " : elem.print p]
                          }
               
instance + Set where
    (+) sl sr = {eval = 'List'.union <$> sl.eval <*> sr.eval, print = \p -> sl.print [" + " : sr.print p]}

instance - Set where
    (-) sl sr = {eval = 'List'.difference <$> sl.eval <*> sr.eval, print = \p -> sl.print [" - " : sr.print p]}
    
instance * Set where
    (*) sl sr = {eval = 'List'.intersect <$> sl.eval <*> sr.eval, print = \p -> sl.print [" * " : sr.print p]}

class Variable a | TC a where
    var :: Ident -> Sem a
    (=.) infixl 2 :: Ident (Sem a) -> Sem a
    
instance Variable Int where
      var i      = {eval = read i, print = \p -> ["valueOf(",i,")" : p]}
      (=.) i sem = {eval = store i sem.eval, print = \p -> [i, " = " : sem.print p]} 
    
instance Variable [Int] where
      var i      = {eval = read i, print = \p -> ["valueOf(",i,")" : p]}
      (=.) i sem = {eval = store i sem.eval, print = \p -> [i, " = " : sem.print p]}  
                
// Boolean Expressions

(In) infix 4 :: Element Set -> Sem Bool
(In) elem set = { eval = isMember <$> elem.eval <*> set.eval
                , print = \p -> elem.print ["In" : set.print p]
                }
                
class (==.) infix 4 a :: a a -> Sem Bool
 
instance ==. Set where
    (==.) setL setR = { eval = (==) <$> setL.eval <*> setR.eval
                      , print = \p ->  setL.print [" == " : setR.print p]
                      }
                    
instance ==. Element where
    (==.) elemL elemR = { eval = (==) <$> elemL.eval <*> elemR.eval
                        , print = \p -> elemL.print [" == " : elemR.print p]
                        }
                                          
(<=.) infix 4 :: Element Element -> Sem Bool
(<=.) elemL elemR = { eval = (<=) <$> elemL.eval <*> elemR.eval
                    , print = \p -> elemL.print [" <= " : elemR.print p]
                    }
                    
(||.) infixr 2 :: (Sem Bool) (Sem Bool) -> Sem Bool
(||.) semL semR = { eval = (||) <$> semL.eval <*> semR.eval
                  , print = \p -> semL.print [" || " : semR.print p]
                  }

(&&.) infixr 3 :: (Sem Bool) (Sem Bool) -> Sem Bool
(&&.) semL semR = { eval = (&&) <$> semL.eval <*> semR.eval
                  , print = \p -> semL.print [" && " : semR.print p]
                  }

Not :: (Sem Bool) -> Sem Bool
Not sem = { eval = not <$> sem.eval
           , print = \p -> ["!" : sem.print p]
           }
          
true :: Sem Bool
true = { eval = pure True, print = \p -> ["True" : p]}

false :: Sem Bool
false = { eval = pure False, print = \p -> ["False" : p]}

// 4. Statements

If :: (Sem Bool) (Sem a) (Sem a) -> Sem a
If cond semTrue semFalse = { eval = cond.eval >>= \condVal -> if condVal semTrue.eval semFalse.eval
                           , print = \p -> ["If (" : cond.print [") \n then " : semTrue.print ["\n else " : semFalse.print p]]]
                           }
                           
For :: Ident Set (Sem a) -> Sem ()
For i set sem = { eval = set.eval >>= \setVal -> case setVal of
                                                     []     = pure ()
                                                     [s:ss] = store i (integer s).eval >>| sem.eval >>| (For i (newSet ss) sem).eval
                , print = \p -> ["Foreach ", i, " in " : set.print [" do \n " : sem.print p]]
                }
                
(:.) infixl 2 :: (Sem a) (Sem b) -> Sem b
(:.) semL semR = { eval = semL.eval >>| semR.eval
                 , print = \p -> semL.print [";\n" : semR.print p]
                 } 

// 5. Printing 
                
prettyPrint :: (Sem a) -> String
prettyPrint sem = concat (sem.print [])


// Test cases for eval
                                                
//Start = eval (integer 2 + integer 3)
//Start = eval (size (newSet [3, 4]))
//Start = eval (integer 2 +. (newSet [3,4,5,6] - newSet [2,1,3,4]))
//Start = eval ((newSet [3,4,5,7] .- integer 4) * (newSet [3,6] .+ integer 7))
//Start = eval ("a" =. (integer 2 +. newSet [3, 4]))
//Start = eval (integer 2 *. newSet [3,4])
//Start = eval ((true &&. false) ||. ((Not false) &&. true))
//Start = eval ((newSet [2, 3]) ==. (newSet [2, 4]))
//Start = eval (newSet [2, 3] +. integer 4)
//Start = eval (If (newSet [2, 3] ==. (integer 2 +. newSet [3])) (integer 2 * integer 3) (integer 3 - integer 2)) 
//Start = eval (If (integer 2 In newSet [3,4]) ("a" =. integer 2) ("a" =. integer 4))
//Start = eval ((For "a" (newSet [4,3]) ("b" =. ((integer 1) + (var "a")))) :. ((var "b") - integer 4))  
//Start = eval (("b" =. integer 2) :. (var "b" + integer 3)) 
Start = eval (("b" =. newSet [2,3]) :. (var "b" + newSet [4,5])) 

// Test cases for prettyPrint

//Start = prettyPrint (If (newSet [2, 3] ==. (integer 2 +. newSet [3])) ("a" =. integer 2) ("a" =. integer 3))
//Start = prettyPrint ("a" =. (newSet [3,4,5,6] - newSet [2,1,3,4]) .+ integer 2) 
//Start = prettyPrint (For "i" (newSet [1,2]) ("a" =. var "i" + integer 10))
//Start = prettyPrint (("b" =. integer 2) :. (var "b" + integer 3))
//Start = prettyPrint ((newSet [3,4] ==. newSet [3,5]) &&. (integer 2 ==. (integer 4 - integer 2)))

