module skeleton8

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
import Data.Tuple
import Data.Either
import qualified Data.List as List
import qualified Data.Map as Map
// use this as: 'List'.union

:: Expression
  = New      [Int]
  | Elem     Int
  | Variable Ident
  | Size     Set
  | (+.) infixl 6 Expression Expression
  | (-.) infixl 6 Expression Expression
  | (*.) infixl 7 Expression Expression
  | (=.) infixl 2 Ident Expression

:: Logical
  = TRUE | FALSE
  | (In) infix 4 Elem Set
  | (==.) infix 4 Expression Expression
  | (<=.) infix 4 Expression Expression
  | Not Logical
  | (||.) infixr 2 Logical Logical
  | (&&.) infixr 3 Logical Logical

:: Stmt
  = Expression Expression
  | Logical Logical
  | For Ident Set Stmt
  | If Logical Stmt Stmt

:: Set    :== Expression
:: Elem  :== Expression
:: Ident  :== String


//1.2
:: Val
  = IntV Int
  | SetV [Int]
  
:: State k v:== Map k v

:: Output a
  = Result a 
  | Fail Message


:: Fail :== String   
:: Sem a = Sem ((State String Val)-> (Either (a,State String Val) Fail))
  
:: Message :== String

instance Functor Sem where
  fmap f (Sem g) = (Sem (\st -> case g st of
                                       Left (a, st`) =  Left ((f a),st`) 
                                       Right s = Right s))

instance Applicative (Sem) where
    pure x = Sem \st -> Left (x, st)
    (<*>) (Sem f) (Sem g) = Sem \st -> case f st of
                                                 Left (f, st) = case g st of 
                                                                    Left (a, st)  = Left ((f a), st)
                                                                    Right s = Right s
                                                 Right s = Right s
	
instance Monad Sem where
    bind (Sem g) f = Sem \st -> case g st of 
                                        Left (a, st)  = unSem (f a) st
                                        Right s = Right s
   
unSem :: (Sem a) -> ((State String Val) -> (Either (a,State String Val) Fail))
unSem (Sem f) = f      

fail :: String -> Sem a
fail s = Sem \st -> (Right s)

read :: Ident -> Sem Val
read s = Sem \st -> case 'Map'.get s st of 
						Just a = Left(a,st)
						Nothing = Right "Could not find associated value"   

store:: Ident Val -> Sem Val
store k v = Sem \st -> Left (v, 'Map'.put k v st)
						

evalE :: Expression -> Sem Val
evalE (New xs) = pure (SetV xs)
evalE (Elem i) = pure (IntV i)
evalE (Variable s) = read s
evalE (Size set) = evalE set >>= \evalSet. case evalSet of
										(SetV [x:xs]) = pure (IntV (length [x:xs]))
										(IntV i)	  = fail "Can't calculate length of an integer"
evalE (ex1 +. ex2) = evalE ex1 >>= \lex.
					 evalE ex2 >>= \rex. case lex of 
                    					(IntV a) = case rex of
                                            			(IntV b) = pure (IntV (a+b))
                                                        (SetV [b:bs]) = pure (SetV [a,b:bs])
                                        (SetV [a:as])  = case rex of 
                                        						(IntV b) = pure (SetV ([a:as] ++ [b]))
                                        						(SetV [b:bs]) = pure (SetV ('List'.union [a:as] [b:bs]))
                                        						
evalE (ex1 -. ex2) = evalE ex1 >>= \lex.
					 evalE ex2 >>= \rex. case lex of 						
											(IntV a) = case rex of
                                            				(IntV b) = pure (IntV (a-b))
                                                        	(SetV [b:bs]) = fail "Can't subtract Set from Int"
                                        	(SetV [a:as])  = case rex of 
                                        						(IntV b) = pure (SetV ('List'.difference [a:as] [b]))
                                        						(SetV [b:bs]) = pure (SetV ('List'.difference [a:as] [b:bs]))
evalE (ex1 *. ex2) = evalE ex1 >>= \lex.
					 evalE ex2 >>= \rex. case lex of 						
											(IntV a) = case rex of
                                            				(IntV b) = pure (IntV (a*b))
                                                        	(SetV [b:bs]) = pure (SetV (map (\v -> v * a) [b:bs]))
                                        	(SetV [a:as])  = case rex of 
                                        						(IntV b) = fail "Can't multiply set with int"
                                        						(SetV [b:bs]) = pure (SetV ('List'.intersect [a:as] [b:bs]))
evalE (ident =. ex) = evalE ex >>= \rex. (store ident rex)


evalL :: Logical -> Sem Bool
evalL TRUE = pure True
evalL FALSE = pure False
evalL (elem In set) = evalE set >>= \evalSet.
				      evalE elem >>= \evalElem. case evalSet of 
				 								     (SetV s) = case evalElem of 
				 												      (IntV i) = pure (isMember i s) 
evalL (ex1 ==. ex2) = evalE ex1 >>= \lex.
				      evalE ex2 >>= \rex. case lex of 						
											(IntV a) = case rex of
                                            				(IntV b) = pure (a == b)
                                                        	_ = pure False
                                        	(SetV a)  = case rex of 
                                        						(SetV b) = pure (a == b)
                                        						_ = pure False
                                        						
evalL (ex1 <=. ex2) = evalE ex1 >>= \lex.
				      evalE ex2 >>= \rex. case lex of 						
											(IntV a) = case rex of
                                            				(IntV b) = pure (a <= b)
                                                        	_ = pure False
                                        	(SetV a)  = case rex of 
                                        						(SetV b) = pure (a <= b)
                                        						_ = pure False
evalL (Not l) = evalL l >>= \log. case log of 
									True = pure False
									False = pure True
evalL (log1 ||. log2) = evalL log1 >>= \bool1.
						evalL log2 >>= \bool2. case (bool1, bool2) of
													(False, False)  = pure False
													_ 				= pure True
evalL (log1 &&. log2) = evalL log1 >>= \bool1.
						evalL log2 >>= \bool2. case (bool1, bool2) of
													(True, True)  = pure True
													_ 				= pure False													

evalS :: Stmt -> Sem ()
evalS (Expression e) = evalE e >>= \val. pure val 
							   >>| pure ()
evalS (Logical l) = evalL l >>= \bool. pure bool
							>>| pure ()
evalS (For i set st) = evalE set >>= \evalSet. case evalSet of
						 							(SetV [])	  = pure ()
						 							(SetV [x:xs]) = (store i (IntV x)) >>| pure (evalS st) >>| pure (evalS (For i (New xs) st)) >>| pure ()	   
evalS (If log st1 st2) = evalL log >>= \bool. case bool of 
													True = pure (evalS st1) >>| pure ()
													_	 = pure (evalS st2) >>| pure ()

print:: Expression -> String
print (New xs) = "( New " +++ toString xs +++  " )"	
print (Elem i) = toString i
print (Variable ident) = "( Variable " +++ ident +++ " )"
print (Size ex) = "( Size" +++ print ex +++ " )"
print (ex1 +. ex2) = print ex1 +++ " + " +++ print ex2
print (ex1 -. ex2) = print ex1 +++ " - " +++ print ex2
print (ex1 *. ex2) = print ex1 +++ " * " +++ print ex2
print (id =. ex2) = id +++ " = " +++ print ex2 		   
						

// === State


// === semantics


// === simulation




(>>>=)     :== tbind
(>>>|) a b :== tbind a (\_ -> b)

test = [1..9]


Start = test
