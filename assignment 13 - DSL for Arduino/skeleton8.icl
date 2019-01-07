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
import StdList
import qualified Data.List as List
import qualified Data.Map as Map
import Text => qualified join
// use this as: 'List'.union


:: INIT 
	= INCLUDE String
	| DEFINE Ident Val
	| VAR Ident Val
	| LCD

:: Setup = Setup
:: Loop = Loop
	
:: Program =  Init [INIT] Setup [Stmt] Loop [Stmt]	

:: Stmt
  = Expression Expression
  | Logical Logical
  | Operation Operation
  | For Ident Expression Logical Expression [Stmt]
  | If Logical [Stmt] [Stmt]
  | Assignment Assignment
  | Break
  
:: Assignment = (=.) infixl 2 Ident Thingie
  
:: Thingie
  = Val Val
  | EXPR Expression
  | LOG Logical
  | FUNC Operation

:: Expression
  = Variable Ident
  | CONST Ident
  | VALUE Val
  | ValueOn Ident Expression
  | (+.) infixl 6 Expression Expression
  | (-.) infixl 6 Expression Expression
  | (*.) infixl 7 Expression Expression

:: Logical
  = TRUE | FALSE
  | (==.) infix 4 Expression Expression
  | (<=.) infix 4 Expression Expression
  | (<.) infix 4 Expression Expression
  | (>=.) infix 4 Expression Expression
  | (>.) infix 4 Expression Expression
  | (!=.) infix 4 Expression Expression
  | Not Logical
  | (||.) infixr 2 Logical Logical
  | (&&.) infixr 3 Logical Logical

:: Operation
  = DigitalWrite Int LedState
  | PinMode      Int Mode
  | SetCursor    Int Int
  | Print        Expression
  | Begin        Int Int
  | AnalogRead   String

:: Mode 
   = INPUT
   | OUTPUT

:: LedState
  = HIGH
  | LOW
     
:: Val
  = INT Int 
  | STRING String
  | BOOL Bool
  | INTL [Int]
  | STRINGL [String]
  | BOOLL [Bool]

:: Ident  :== String

:: State = { cursor :: (Int,Int)
           , begin :: (Int,Int)
           , ledStates :: 'Map'.Map Int (LedState,Mode)
           , print :: [String]
           , store :: 'Map'.Map Ident Val
           , constants :: 'Map'.Map Ident Val
           , lcd :: Bool
           , buttonPressed :: Int
           }
 
:: Message :== String
 
:: Output a 
 = Result a
 | Fail Message
 
:: Sem a = Sem (State -> (Output a,State))
 
instance toString Val where
	toString (INT i) = toString i
	toString (BOOL b) = case b of
							True = "True"
							False = "False"  
	toString (STRING s) = s
	toString (INTL xs) = toString xs
	toString (BOOLL xs) =  concat (map (\b -> toString b) xs)
	toString (STRINGL xs) = concat xs
  
instance Functor Sem where
  fmap f (Sem g) = (Sem (\st -> case g st of
                                       (Result a, st`) =  (Result (f a), st`) 
                                       (Fail s, st`) = (Fail s, st`)))

instance Applicative (Sem) where
    pure x = Sem \st ->  (Result x, st)
    (<*>) (Sem f) (Sem g) = Sem \st -> case f st of
                                                 (Result f, st`) = case g st` of 
                                                                    (Result a, st2)  = (Result (f a), st2)
                                                                    (Fail s, st2) = (Fail s, st2)
                                                 (Fail s, st`) = (Fail s, st`)
	
instance Monad Sem where
    bind (Sem g) f = Sem \st -> case g st of 
                                        (Result a, st`)  = unSem (f a) st`
                                        (Fail s, st`) = (Fail s, st`)
   
unSem :: (Sem a) -> (State -> (Output a,State))
unSem (Sem f) = f

fail :: String -> Sem a
fail s = Sem \st -> (Fail s, st)

read :: Ident -> Sem Val
read s = Sem \st -> case 'Map'.get s st.store of 
						Just a = (Result a,st)
						Nothing = (Fail "Could not find associated value", st)   

readFromConstants :: Ident -> Sem Val
readFromConstants s = Sem \st -> case 'Map'.get s st.constants of 
						Just a =  (Result a,st)
						Nothing = (Fail "Could not find associated value", st)
						
constantDefined :: Ident -> Sem Bool
constantDefined s = Sem \st -> case 'Map'.get s st.constants of 
						Just a =  (Result True,st)
						Nothing = (Result False, st)

store:: Ident Val -> Sem Val
store k v = Sem \st -> (Result v,{st & store = ('Map'.put k v st.store)})

removeFromStore:: Ident -> Sem ()
removeFromStore i = Sem \st -> (Result (),{st & store = ('Map'.del i st.store)})

enforceInt:: (Sem Val) -> Sem Int
enforceInt v = v >>= \val. case val of 
							(INT i) = pure i
							_       = fail ("Can't parse to int")

evalE :: Expression -> Sem Val
evalE (Variable ident) = read ident
evalE (VALUE v) = pure v
evalE (CONST ident) = readFromConstants ident
evalE (lexp +. rexp) = evalE lexp >>= \n. 
                         evalE rexp >>= \m.
                         case (n,m) of
                       		 (INT i, INT k)      = pure (INT (i + k))
                       		 (STRING s, STRING t) = pure (STRING (s +++ t))
                       		 _                    = fail ("No operator + defined for these types")
evalE (lexp -. rexp) = evalE lexp >>= \n. 
                       evalE rexp >>= \m.
                       case (n,m) of
                       		(INT i, INT k)      = pure (INT (i - k))
                       		_                    = fail ("No operator - defined for these types")
evalE (lexp *. rexp) = evalE lexp >>= \n. 
                       evalE rexp >>= \m.
                       case (n,m) of
                       		(INT i, INT k)      = pure (INT (i * k))
                       		_                    = fail ("No operator * defined for these types")
evalE (ValueOn i e) = enforceInt (evalE e) >>= \int.
					  read i >>= \list. case list of
					  						(INTL il) = pure (INT (il !! int))
					  						(BOOLL bl) = pure (BOOL (bl !! int))
					  						(STRINGL sl) = pure (STRING (sl !! int))
					  

evalL :: Logical -> Sem Bool
evalL TRUE = pure True
evalL FALSE = pure False
evalL (ex1 ==. ex2) = evalE ex1 >>= \lexp.
				      evalE ex2 >>= \rexp. case (lexp, rexp) of 						
											    (INT i, INT k) = pure (i == k)
											    (STRING i, STRING k) = pure (i == k)
											    (BOOL i, BOOL k) = pure (i == k)
											    (INTL i, INTL k) = pure (i == k)
											    (STRINGL i, STRINGL k) = pure (i == k)
											    (BOOLL i, BOOLL k) = pure (i == k)
evalL (ex1 <. ex2) = evalE ex1 >>= \lexp.
				     evalE ex2 >>= \rexp. case (lexp, rexp) of 						
											   (INT i, INT k) = pure (i < k)
											   (STRING i, STRING k) = pure (i < k)
											   (INTL i, INTL k) = pure (i < k)
											   (STRINGL i, STRINGL k) = pure (i < k)
evalL (ex1 <=. ex2) = evalE ex1 >>= \lexp.
				      evalE ex2 >>= \rexp. case (lexp, rexp) of 						
											    (INT i, INT k) = pure (i <= k)
											    (STRING i, STRING k) = pure (i <= k)
											    (INTL i, INTL k) = pure (i <= k)
											    (STRINGL i, STRINGL k) = pure (i <= k)
evalL (ex1 >=. ex2) = evalE ex1 >>= \lexp.
				      evalE ex2 >>= \rexp. case (lexp, rexp) of 						
											    (INT i, INT k) = pure (i >= k)
											    (STRING i, STRING k) = pure (i >= k)
											    (INTL i, INTL k) = pure (i >= k)
											    (STRINGL i, STRINGL k) = pure (i >= k)
evalL (ex1 >. ex2) =  evalE ex1 >>= \lexp.
				      evalE ex2 >>= \rexp. case (lexp, rexp) of 						
											    (INT i, INT k) = pure (i > k)
											    (STRING i, STRING k) = pure (i > k)
											    (INTL i, INTL k) = pure (i > k)
											    (STRINGL i, STRINGL k) = pure (i > k)
								
evalL (ex1 !=. ex2) = evalE ex1 >>= \lexp.
				      evalE ex2 >>= \rexp. case (lexp, rexp) of 						
											    (INT i, INT k) = pure (not (i == k))
											    (STRING i, STRING k) = pure (not (i == k))
											    (BOOL i, BOOL k) = pure (not (i == k))
											    (INTL i, INTL k) = pure (not (i == k))
											    (STRINGL i, STRINGL k) = pure (not (i == k))
											    (BOOLL i, BOOLL k) = pure (not (i == k))							    											    											    
evalL (Not l) = evalL l >>= \log. pure (log == False)
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
evalS (If log st1 st2) = evalL log >>= \bool. case bool of 
													True = pure (evalStmts st1) >>| pure ()
													_	 = pure (evalStmts st2) >>| pure ()
evalS (For ident expr1 logical expr2 statements) = evalE expr1 >>= \initVal. 
												   store ident initVal >>| 
								  		           evalL logical >>= \bool. case bool of
																True = evalStmts statements >>= \b. 
																	   case b of 
																	        False = removeFromStore ident >>| pure ()
																	        True  = evalE expr2 >>= \i. 
																	        		evalS (For ident (VALUE i) logical expr2 statements)
																_    = removeFromStore ident >>| pure ()
evalS (Operation o) = evalO o >>| pure ()
evalS (Assignment a) = evalA a >>= \val. pure val
							   >>| pure ()
evalS (Break) = pure ()

evalStmts :: [Stmt] -> Sem Bool
evalStmts [] = pure True
evalStmts [Break:_] = pure False
evalStmts [x:xs]    = evalS x >>| evalStmts xs

evalO :: Operation -> Sem Val
evalO (DigitalWrite i newS) = Sem \st -> case 'Map'.get i st.ledStates of 
												Just (lst,m)  = (Result (STRING ""),{st & ledStates = ('Map'.put i (newS,m) st.ledStates)})
												_        		   = (Fail "Cannot write to a pin that is not initialized", st)
evalO (PinMode i m) = Sem \st -> case 'Map'.get i st.ledStates of 
												Just (lst,oldMode)  = (Result (STRING ""),{st & ledStates = ('Map'.put i (lst,m) st.ledStates)})
												_              			 = (Result (STRING ""),{st & ledStates = ('Map'.put i (LOW,m) st.ledStates)})	
evalO (SetCursor x y) = Sem \st -> (Result (STRING ""),{st & cursor = (x,y)})
evalO (Print expr)    = evalE expr >>= \val. Sem \st -> (Result (STRING ""),{st & print = [toString val:st.print]})
evalO (Begin x y)     = Sem \st -> (Result (STRING ""),{st & begin = (x,y)})
evalO (AnalogRead i)  = Sem \st -> (Result (INT (st.buttonPressed)),st)

evalA :: Assignment -> Sem ()
evalA (i =. t) = evalT t >>= \th.
				 store i th >>| pure ()

evalT :: Thingie -> Sem Val
evalT (Val v) = pure v
evalT (EXPR e) = evalE e
evalT (LOG l) = evalL l >>= \bool.
				pure (BOOL bool)
evalT (FUNC o) = evalO o >>= \val.
				 pure val

evalI :: INIT -> Sem ()
evalI (INCLUDE s) = Sem \st -> (Result (),{st & print = ["#include ":st.print]})
evalI (DEFINE i v) = (constantDefined i) >>= \b.
					 			case b of
					 	  			 True =  Sem \st -> (Fail "Cannot overwrite constant value",st)
					 	  			 False = Sem \st -> (Result (),{st & constants = ('Map'.put i v st.constants)})
evalI (VAR i v) = Sem \st -> (Result (),{st & store = ('Map'.put i v st.store)})
evalI (LCD) = Sem \st -> (Result (),{st & lcd = True})																				

evalP :: Program -> Sem ()
evalP (Init [i:is] Setup s Loop l) = evalI i >>| evalP (Init is Setup s Loop l)
evalP (Init _ Setup [s:ss] Loop l) = evalS s >>| evalP (Init [] Setup ss Loop l)
evalP (Init _ Setup _ Loop [l:ls]) = evalS l >>| evalP (Init [] Setup [] Loop ls)
evalP (Init _ Setup _ Loop _) = pure ()

initialState = { cursor = (0,0),
			     begin = (0,0),
			     ledStates = 'Map'.newMap,
			     print = [],
			     store = 'Map'.newMap,
			     constants = 'Map'.newMap,
			     lcd = False,
			     buttonPressed = 0
			   }

testCase1 = unSem (evalP (
				  Init 	[ INCLUDE "<LiquidCrystal.h>",
				  		  DEFINE "KEY_COUNT" (INT 5),
				  		  VAR "keyLimits" (INTL [ 50,
				  		  						  190,
				  		  						  380,
				  		  						  555,
				  		  						  790,
				  		  						  1024
				  		  				        ]
				  		  				  ),
				  		  VAR "keyNames" (STRINGL [ "Right ",
				  		  							"Up   ",
				  		  							"Down  ",
				  		  							"Left  ",
				  		  							"Select",
				  		  							"No key"
				  		  					      ]
				  		  			 	 ),
				  		  LCD
				  		]
				  Setup [ Operation (Begin 16 2)
				  		]
				  Loop [ Assignment ("val" =. FUNC (AnalogRead "A0")),
				  		 Operation (SetCursor 0 0),
				  		 Operation (Print (Variable "val")),
				  		 Operation (Print (VALUE (STRING "val"))),
				  		 For "i" (VALUE (INT 0)) ((Variable "i") <=. (CONST "KEY_COUNT")) ((Variable "i") +. (VALUE (INT 1))) [ 
				  		 																									If ((Variable "val") <. (ValueOn "keyLimits" (Variable "i"))) [
				  		 																																					     	Operation (SetCursor 0 1),
				  		 																																						 	Operation (Print (ValueOn "keyNames" (Variable "i"))),
				  		 																																						 	Break
				  		 																																					       ]
				  		 																																					       []
				  		 																									]
				  	   ]
				  )) initialState

Start = testCase1
