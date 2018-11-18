// Brigel Pineti s1005549
// Tim Turksema s1013838

module skeletonDeepEmbedded

/*
  Advanved Progrmming 2018, Assignment 8
  Pieter Koopman, pieter@cs.ru.nl
*/

import iTasks => qualified return, >>=, >>|, sequence, forever, :: Set
/*
	qualified import of the named objects to avoid name conflicts.
	Use this as 'iTasks'.return. All other parts of iTasks are available.
*/
import Data.Functor, Control.Applicative, Control.Monad
import Data.Tuple, Data.Either
import GenPrint

import qualified Data.List as List
import qualified Data.Map as Map
// use this as: 'List'.union

// 1.1

:: Expression
  = New      [Int]
  | Elem     Int
  | Variable Ident
  | Size     Set
  | (+.) infixl 6 Expression Expression
  | (-.) infixl 6 Expression Expression
  | (*.) infixl 7 Expression Expression
  | (=.) infixl 2 Ident Expression

:: Set    :== Expression
:: Elem  :== Expression
:: Ident  :== String

// === State

// 1.2

:: Val = IntV Int | SetV [Int]

:: State :== 'Map'.Map Var Val
:: Var :== String
  
// 1.3

:: Sem a = Sem (State -> Either Fail (a, State))

:: Fail :== String

unSem :: (Sem a) -> State -> Either Fail (a, State)
unSem (Sem s) = s

instance Functor Sem where
  //fmap :: (a->b) (Sem a) -> (Sem b)
    fmap atob (Sem g) = Sem \st -> case g st of
                                    (Left m)        = Left m
                                    (Right (a, st)) = Right ((atob a), st)
    
instance Applicative Sem where
  //pure :: a -> Sem a  
    pure x = Sem \st -> Right (x, st)
  //(<*>) infixl 4 :: (Sem (a->b)) (Sem a) -> Sem b
  //(<*>) ff ss = ff >>= \f -> ss >>= \s -> pure (f s)     ...Possible to use monadic bind to construct <*>...
    (<*>) (Sem f) (Sem g) = Sem \st -> case f st of
                                           (Right (atob, st)) = case g st of
                                                                 (Right (a, st)) = Right (atob a, st)
                                                                 (Left m)        = Left m 
                                           (Left m)           = Left m
    
instance Monad Sem where
  //bind :: (m a) (a->m b) -> m b
    bind (Sem g) atomb = Sem \st -> case g st of
                                (Left m)        = Left m
                                (Right (a, st)) = unSem (atomb a) st  
                                
store :: Ident Val -> Sem Val
store i v = Sem \st -> Right (v, 'Map'.put i v st)

read :: Ident -> Sem Val
read i = Sem \st -> case ('Map'.get i st) of 
                        Just v  = Right (v, st)
                        Nothing = Left ("Variable " +++ i +++ " not found in the store")

fail :: String -> Sem Val
fail m = Sem \_ -> Left m

// 1.4

eval :: Expression -> Sem Val
eval (New set)    = pure (SetV set)
eval (Elem e)     = pure (IntV e)
eval (Variable i) = read i
eval (Size expr)  = eval expr >>= \exprVal -> case exprVal of
                                                  (SetV set) = pure (IntV (length set))
                                                  (IntV val) = fail "Error, you are trying to find the size of an interger"
eval (el +. er)   = eval el >>= \elVal -> eval er >>= \erVal -> case elVal of
                                                                    (IntV vl) = case erVal of
                                                                                    (IntV vr) = pure (IntV (vl + vr))
                                                                                    (SetV sr) = pure (SetV ('List'.union [vl] sr))
                                                                    (SetV sl) = case erVal of
                                                                                    (IntV vr) = pure (SetV ('List'.union sl [vr]))
                                                                                    (SetV sr) = pure (SetV ('List'.union sl sr))
eval (el -. er)   = eval el >>= \elVal -> eval er >>= \erVal -> case elVal of
                                                                    (IntV vl) = case erVal of
                                                                                    (IntV vr) = pure (IntV (vl - vr))
                                                                                    (SetV sr) = fail "Error, cannot find difference between an Int and a list of Int"
                                                                    (SetV sl) = case erVal of
                                                                                    (IntV vr) = pure (SetV ('List'.difference sl [vr]))
                                                                                    (SetV sr) = pure (SetV ('List'.difference sl sr))
eval (el *. er)   = eval el >>= \elVal -> eval er >>= \erVal -> case elVal of
                                                                    (IntV vl) = case erVal of
                                                                                    (IntV vr) = pure (IntV (vl * vr))
                                                                                    (SetV sr) = pure (SetV (map ((*) vl) sr)) 
                                                                    (SetV sl) = case erVal of
                                                                                    (IntV vr) = fail "Error, cannot apply *. from a list of Int to an Int"
                                                                                    (SetV sr) = pure (SetV ('List'.intersect sl sr))                                             
eval (i =. expr)  = eval expr >>= \exprVal -> store i exprVal

  
// 2.1 Logical Expressions
 
:: Logical
  = TRUE | FALSE
  | (In) infix 4 Elem Set
  | (==.) infix 4 Expression Expression
  | (<=.) infix 4 Expression Expression
  | Not Logical
  | (||.) infixr 2 Logical Logical
  | (&&.) infixr 3 Logical Logical
  
instance == Val where
    (==) (IntV vl) (IntV vr) = vl == vr
    (==) (SetV sl) (SetV sr) = (sort sl) == (sort sr)
    (==) _ _                 = False
    
instance < Val where
    (<) (IntV vl) (IntV vr) = vl <= vr
    (<) _ _                 = False
  
evalL :: Logical -> Sem Bool
evalL TRUE        = pure True
evalL FALSE       = pure False
//evalL (ee In ss)  = eval ee >>= \eeVal -> eval ss >>= \ssVal -> case eeVal of
//                                                                     (IntV vl)  = case ssVal of
//                                                                                     (SetV sr) = pure (isMember vl sr)
//                                                                                     (IntV vr) = fail "Error, can't check if Int is a member of Int"
//                                                                     _ = fail "Error, can't check if Set is a member of a Set or Int"
evalL (el ==. er) = eval el >>= \elVal -> eval er >>= \erVal -> pure (elVal == erVal)
evalL (el <=. er) = eval el >>= \elVal -> eval er >>= \erVal -> pure (elVal < erVal)
evalL (Not log)   = evalL log >>= \logVal -> pure (not logVal)
evalL (ll ||. lr) = evalL ll >>= \llVal -> evalL lr >>= \lrVal -> pure (llVal || lrVal) //Problem
evalL (ll &&. lr) = evalL ll >>= \llVal -> evalL lr >>= \lrVal -> pure (llVal && lrVal) //Problem

// 2.2 Statements

:: Stmt
  = Expression Expression
  | Logical Logical
  | For Ident Set Stmt
  | If Logical Stmt Stmt 
  
:: StmtVal = Expr Val | Log Bool
  
evalS :: Stmt -> Sem StmtVal
evalS (Expression expr)     = eval expr >>= \exprVal -> pure (Expr exprVal)
evalS (Logical log)         = evalL log >>= \logVal -> pure (Log logVal)
evalS (If cond stmtT stmtF) = evalL cond >>= \condVal -> case condVal of
                                                           True  = evalS stmtT
                                                           False = evalS stmtF
evalS (For i set stmt)      = eval set >>= \setVal -> case setVal of
                                                          (SetV sV) = forEach i sV stmt
                                                          (IntV iV) = forEach i [0..iV] stmt
                                                          
forEach :: Ident [Int] Stmt -> Sem StmtVal
forEach i set stmt = foldr seqStmt (pure (Log True)) (map (exec stmt i) set)

exec :: Stmt Ident Int -> Sem StmtVal
exec stmt i val = store i (IntV val) >>= \_ -> evalS stmt

seqStmt :: (Sem StmtVal) (Sem StmtVal) -> Sem StmtVal
seqStmt (Sem f) (Sem g) = Sem \st -> f st >>= \(_, newSt) -> g newSt 

// 3. Printing

class printing a :: a -> String

instance printing Ident where 
    printing id = id
    
instance printing Expression where
    printing (New set)     = " New: " +++ toString set
    printing (Elem e)      = " Elem: " +++ toString e
    printing (Variable id) = " Variable: " +++ id
    printing (Size expr)   = " Size: " +++ printing expr
    printing (el +. er)    = " " +++ printing el +++ " + " +++ printing er
    printing (el -. er)    = " " +++ printing el +++ " - " +++ printing er
    printing (el *. er)    = " " +++ printing el +++ " * " +++ printing er
    printing (el =. er)    = " " +++ printing el +++ " = " +++ printing er
    
instance printing Logical where
    printing TRUE        = "True"
    printing FALSE       = "False"
    printing (e In s)    = " " +++ printing e +++ " in " +++ printing s
    printing (el ==. er) = " " +++ printing el +++ " == " +++ printing er
    printing (el <=. er) = " " +++ printing el +++ " <= " +++ printing er
    printing (Not log)   = " (Not " +++ printing log +++ ") "
    printing (el ||. er) = " " +++ printing el +++ " || " +++ printing er
    printing (el &&. er) = " " +++ printing el +++ " && " +++ printing er
         
instance printing Stmt where
    printing (Logical log)         = " " +++ printing log
    printing (Expression expr)     = " " +++ printing expr
    printing (If cond stmtT stmtF) = " If " +++ printing cond +++ " then " +++ printing stmtT +++ " else " +++ printing stmtF
    printing (For i set stmt)      = " For " +++ i +++ " in " +++ printing set +++ " do " +++ printing stmt +++ " od "    

instance printing StmtVal where
    printing (Expr v) = " Expr " +++ printing v
    printing (Log l)  = " Log  " +++ toString l 
    
instance printing Val where
    printing (IntV int) = " IntV " +++ toString int
    printing (SetV set) = " SetV " +++ toString set
    
// 4. Simulation

(>>>=)     :== tbind
(>>>|) a b :== tbind a (\_ -> b)

derive class iTask Expression, Logical, Stmt, Val, StmtVal

startSimulator :: Task State
startSimulator = enterInformation "Enter the input" [] >>>= simulate state
                 where 
                     state = 'Map'.newMap
                     
simulate :: State Stmt -> Task State   
simulate state stmt = case (unSem (evalS stmt)) state of
                         (Left m) = viewInformation "Evaluation error" [] m >>>| simulate state (Logical TRUE)
                         (Right (val, state)) = (enterInformation "Enter new input" []
                                             -|| viewInformation "Value" [] (printing val)
                                             -|| viewInformation "State" [] state
                                             -|| viewInformation "Print" [] (printing stmt))
                                             >>* [ OnAction (Action "Continue") (hasValue (simulate state))
                                                 , OnAction (Action "Reset State") (always (simulate ('Map'.newMap) stmt))
                                                 , OnAction (Action "Quit") (always startSimulator)
                                                 ]
                                                 
Start :: *World -> *World
Start w = startEngine startSimulator w
