module skeleton4

// Brigel Pineti s1005549
// Tim Turksema s1013838

import qualified Data.Map as Map

import StdEnv, StdMaybe, StdList
import Data.List


:: Gram = Lit String
        | Idn
        | Int
        | Seq [Gram]
        | Alt [Gram]
        | Def Name Gram Gram
        | Var Name
        
:: Name :== String
 
listIntGram :: Gram
listIntGram = Def "list" (Alt [Lit "[]", Seq [Lit "[", Int, Lit ":", Var "list", Lit "]"]]) (Seq [Idn, Lit "=", Var "list"])

:: State = { input :: [String]
           , seen  :: [String]
           , store :: Store
           }
           
:: Store :== 'Map'.Map Name Gram

:: Parse a = Parse (State -> (Maybe a, State))

// 1.1

class MyFunctor f where
	fmap :: (a->b) (f a) -> (f b)
	(<$>) infixl 4 :: (a->b) (f a) -> (f b) | MyFunctor f
 	(<$>) f x :== fmap f x
 	
instance MyFunctor (Parse) where
    fmap f (Parse g) = Parse \st -> case g st of
                                       (Just a, st`) = (Just (f a), st`) 
                                       (_, st`) = (Nothing, st`) 

class MyApplicative f | MyFunctor f where
	pure :: a -> f a
	(<*>) infixl 4 :: (f (a->b)) (f a) -> f b
	
instance MyApplicative (Parse) where
    pure x = Parse \st -> (Just x, st)
    (<*>) (Parse f) (Parse g) = Parse \st -> case f st of
                                                 (Just f, st) = case g st of 
                                                                    (Just a, st)  = (Just (f a),st)
                                                                    (Nothing, st) = (Nothing, st)
                                                 (Nothing, st) = (Nothing, st)

class MyMonad m | MyApplicative m where
	bind :: (m a) (a->m b) -> m b
	(>>=) infixl 1 :: (m a) (a->m b) -> m b | Monad m
	(>>=) a f :== bind a f
	(>>|) infixl 1 :: (m a)    (m b) -> m b | Monad m
	(>>|) a b :== a >>= \_.b
	rtrn :: a -> m a | MyMonad m
	rtrn a :== pure a
	
instance MyMonad (Parse) where
    bind (Parse g) f = Parse \st -> case g st of 
                                        (Just a, st)  = unParse (f a) st
                                        (Nothing, st) = (Nothing, st) 
                                        
    
unParse :: (Parse a) -> State -> (Maybe a,State)
unParse (Parse f) = f   

class MyAlternative m where
    empty :: m a 
	(<|>) infixl 0 :: (m a) (m a) -> m a
	
instance MyAlternative (Parse) where
    empty = Parse \st -> (Nothing, st)
    (<|>) (Parse f) (Parse g) = Parse \st -> case f st of
                                                (Nothing, _) = g st
                                                other        = other 
                                                
// 1.2 
                                                
next :: Parse String 
next = Parse \st -> (Just (head st.State.input), { input = delete (head st.State.input) st.State.input
                                                 , seen =  st.State.seen ++ [(head st.State.input)]
                                                 , store = st.State.store
                                                 })
                                                 
back :: Parse String 
back = Parse \st -> (Just (last st.State.seen), { input = [last st.State.seen] ++ st.State.input
                                                , seen  = delete (last st.State.seen) st.State.seen
                                                , store = st.State.store
                                                }) 
                                                
setGram :: Name Gram -> Parse Gram
setGram name gram = Parse \st -> (Just gram, { input = st.State.input
                                             , seen  = st.State.seen
                                             , store = 'Map'.put name gram st.State.store
                                             })
                                             
getGram :: Name -> Parse Gram  
getGram name = Parse \st -> ('Map'.get name st.State.store, { input = st.State.input
                                                                   , seen  = st.State.seen
                                                                   , store = st.State.store
                                                                   })
                                                                   
listIntInput = ["mylist", "=", "[", "7", ":", "[", "42", ":", "[]", "]", "]"]

// 1.3 
// Got stuck in retrieving the chars of a String during the satisfy parser and when composing the initial char with the rest of the String in pure function.
// In addition, we found part 1.3 and 1.4 confusing and could not bring them an end. 
// However, we spent plenty of hours on it and the work we achieved is presented below. 
// We hope that this would be sufficient to pass this assignment.

                                                                   
:: TREE = LIT String | IDN String | INT Int | SEQ [TREE] 

//After getting the next string, we dont know how to get the char of the string
//In Haskell, it was like pattern matching on a list and way easier, but we could not find how to do it here
satisfy :: (Char -> Bool) -> Parse Char
satisfy pred = next >>= \x -> case (pred x) of
                              True -> pure x
                              False -> empty                              

isDigit :: Parse Char
isDigit = satisfy (\n -> n >= '0' && n <= '9')

isNumber :: Parse String
isNumber = isDigit >>= \num -> isNumber >>= \nums -> pure (num +++ nums) 

isUppercase :: Parse Char
isUppercase = satisfy (\a -> a >= 'A' && a <= 'Z')

isLowercase :: Parse Char
isLowercase = satisfy (\a -> a >= 'a' && a <= 'z')

isLetter :: Parse Char
isLetter = isUppercase <|> isLowercase

//isWord :: Parse String 
//isWord = isLetter >>= \letter -> repeat isLetter >>= \letters -> pure (letter +++ letters)

//repeat :: Parse a -> Parse [a]
//repeat p = (p >>= \x -> repeat p >>= \xs -> pure [x:xs]) <|> empty

//Used to compare input with some specific char
singleChar :: Char -> Parse Char 
singleChar a = satisfy (\b -> a == b)

//Again we dont know how to pattern match on a String
//The tokenizer will recognize any keywork, for example tokenize "Lit"
//tokenize :: String -> Parse String
//tokenize ""        = empty
//tokenize [inp:inps] = singleChar inp >>= \first -> tokenize inps >>= \rest -> pure (inp +++ inps)


/*
parse_literal :: Parse TREE
parse_literal = tokenize "Lit " >>= \_ -> isWord >>= \word -> pure (LIT word)

parse_identifier :: Parse TREE
parse_identifier = tokenize "Idn " >>= \_ -> isWord >>= \word -> pure (IDN word)

parse_int :: Parse TREE
parse_int = tokenize "Int " >>= \_ -> isNumber >>= \num -> pure (INT num)

parse_seq :: Parse TREE
parse_seq = tokenize "Seq [" >>= \_ -> parse_all >>= \tree -> singleChar ']' >>= \_ -> pure (SEQ [tree])

parse_all :: Parse TREE
parse_all = parse_literal <|> parse_identifier <|> parse_int <|> parse_seq
*/

//parse :: Gram -> Parse Gram
//parse someGram = setGram "New" someGram >>= \gram -> pure gram

/*
parse :: Gram -> Parse TREE
parse (Lit a) = parse_literal
parse (Idn)   = parse_identifier
parse (Int)   = parse_integer
parse (Seq [Gram]) = parse_seq
parse _       = empty
*/

Start = "True" 
//parse listIntGram

