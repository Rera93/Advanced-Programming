// Brigel Pineti s1005549
// Tim Turksema s1013838

implementation module gastyStart

/*
	Pieter Koopman, Radboud University, 2016, 2017
	pieter@cs.ru.nl
	Advanced programming
	A simplified MBT tool based on logical properties
	
	Use the iTask environment!
	Execute with "Basic values only" option
*/

import StdEnv, StdGeneric, Data.GenEq, Data.List
import cashModel
derive class testArg Product

test :: p -> [String] | prop p
test p = check 1000 (holds p prop0)

check :: Int [Prop] -> [String]
check n [] = ["Proof\n"]
check 0 l  = ["Passed\n"]
check n [p:x] | p.bool
	= check (n-1) x
	= ["Fail for: ": reverse ["\n":p.info]]

class prop a where holds :: a Prop -> [Prop]

instance prop Bool where holds b p = [{p & bool = b}]

// crashes the iTask compiler if there is no dcl module :(
instance prop (a->b) | prop b & testArg a 
where
	holds f p = diagonal [holds (f a) {p & info = [" ",string{|*|} a:p.info]} \\ a <- gen{|*|}]

class testArg a | gen{|*|}, string{|*|}, gEq{|*|} a 

:: Prop =
	{ bool :: Bool
	, info :: [String]
	}
prop0 = {bool = True, info = []}

generic gen a :: [ a ]
gen{|Int|}  = [0,1,-1,maxint,minint,maxint-1,minint+1:[j\\i<-[2..], j<-[i,~i]]]
gen{|Real|} = [0.0, 1.0, -1.0: reals]
where
	reals = [r \\ (x,y) <- diag2 ints ints | x <> y, r <- let c = toReal x / toReal y in [c, ~c]]
	ints = [1: sieve [2..]]
	sieve [p:xs] = [p: sieve [x \\ x <- xs | x rem p <> 0]]
gen{|Bool|} = [True,False]
gen{|Char|} = [' '..'~'] ++ ['\t\n\b']
gen{|UNIT|} = [UNIT]
gen{|PAIR|}   f g	= map (\(a,b)=PAIR a b) (diag2 f g)
gen{|EITHER|} f g = merge (map RIGHT g) (map LEFT f)
where
  merge [a:x] ys = [a: merge ys x]
  merge []    ys = ys
gen{|CONS|}   f  = map CONS f
gen{|OBJECT|} f  = map OBJECT f
gen{|RECORD|} f  = map RECORD f
gen{|FIELD|}  f  = map FIELD f

generic string a :: a -> String
string{|Int|}  i = toString i
string{|Real|} x = toString x
string{|Bool|} b = toString b
string{|Char|} c = toString ['\'',c,'\'']
string{|UNIT|} _ = ""
string{|PAIR|} f g (PAIR x y) = f x + " " + g y
string{|EITHER|} f g (LEFT x) = f x
string{|EITHER|} f g (RIGHT y) = g y
string{|CONS of gcd|} f (CONS x) | gcd.gcd_arity > 0
	= "(" + gcd.gcd_name + " " + f x + ")"
	= gcd.gcd_name
string{|OBJECT|} f (OBJECT x) = f x
string{|RECORD of grd|} f (RECORD x) = "{" + grd.grd_name + "|" + f x + "}"
string{|FIELD of gfd|} f (FIELD x) = gfd.gfd_name + " = " + f x + " "

maxint :: Int
maxint =: IF_INT_64_OR_32 (2^63-1) (2^31-1) //2147483647

minint :: Int
minint =: IF_INT_64_OR_32 (2^63) (2^31) //-2147483648

instance + String where + s t = s +++ t

diagonal :: [[a]] -> [a]
diagonal list = f 1 2 list []
where
	f n m [] [] = []
	f 0 m xs ys = f m (m+1) (rev ys xs) []
	f n m [] ys = f m (m+1) (rev ys []) []
	f n m [[x:r]:xs] ys = [x: f (n-1) m xs [r:ys]]
	f n m [[]:xs] ys = f (n-1) m xs ys
	
	rev []    accu = accu
	rev [x:r] accu = rev r [x:accu]
	
// 1. Specific Test Cases

pUpper :: Char -> Bool
pUpper c = not (c == toUpper c)

:: For a p = (For) infix 6 (a -> p) [a] & prop p & testArg a

instance prop (For a p) where
    holds (f For list) p = diagonal [holds (f a) {p & info = [" ", string{|*|} a : p.info]} \\ a <- list]
    
//Start = ["pUpper : " : check 25 (holds (pUpper For ['a'..'z']) prop0)]
// ["pUpper : ","Passed"]

//Start = ["pUpper : " : test (pUpper For ['a'..'z'])] 
// ["pUpper : ","Proof"]

// 2. Input Selection

:: InputSelection p = (==>) infix 3 Bool p & prop p  

instance prop (InputSelection p) where
    holds (c ==> a) p = if c (holds a p) []   
    
//Start = ["pUpper lower : " : test (\c -> isLower c ==> pUpper c)]
// ["pUpper lower : ","Proof"]

// 3. Tracing the Arguments of Equality

:: Equality a = (=.=) infix 1 a a & testArg a

instance prop (Equality a) where
    holds (l =.= r) p = [{p & bool = gEq{|*|} l r, info = [" Left Side and Right Side are not Equal :>> " +++ string {|*|} l +++ " != " +++ string {|*|} r : p.info]}]

// 4. Testing (Euro)

derive gen Action, Euro, []
derive string Action, Euro, []
derive bimap []

pPlusCommutative :: Euro Euro -> Equality Euro // Return type: Bool for ==
pPlusCommutative euroL euroR = euroL + euroR =.= euroR + euroL

//Start = ["pPlusCommutative :" : test pPlusCommutative]
// ["pPlusCommutativity: ","Passed"]

pMinusCommutative :: Euro Euro -> Equality Euro // Return type: Bool for ==
pMinusCommutative euroL euroR = euroL - euroR =.= ~(euroR - euroL)

//Start = ["pMinusCommutative :" : test pMinusCommutative]
/* ["pMinusCommutativityAbs: ","Fail for: ","{Euro|euro = 1  cent = 0 }"," ","{Euro|euro = 1  cent = -9223372036 }"," ","
     Left Side and Right Side are not Equal :>> {Euro|euro = -92233720368547758  cent = -8} != {Euro|euro = 92233720368547758  cent = -8}",""]  */

pPlusZeroId :: Euro -> Equality Euro // Return type: Bool for ==
pPlusZeroId euro = zero + euro =.= euro

//Start = ["pPlusZeroId :" : test pPlusZeroId]
/* ["pPlusZeroId: ","Fail for: ","{Euro|euro = 0  cent = 1 }"," ","Left Side and Right Side
   are not Equal :>> {Euro|euro = 0  cent = 0} != {Euro|euro = 0  cent = 1 }",""]  */

pMinusZeroId :: Euro -> Equality Euro // Return type: Bool for ==
pMinusZeroId euro = zero - euro =.= ~euro

//Start = ["pMinusZeroId :" : test pPlusCommutative]
// ["pMinusZeroId: ","Passed"]

pDoubleNeg :: Euro -> Equality Euro // Return type: Bool for ==
pDoubleNeg euro = euro =.= ~(~euro)

//Start = ["pDoubleNeg :" : test pDoubleNeg]
// ["pDoubleNeg: ","Passed"]

// 5. Testing the Remove Action in the Model

pValueAfterRem :: [Product] Product -> Bool
pValueAfterRem cart p = case (model cart (Rem p)) of 
								(l, _) = (model l Pay) == case isMember p cart of
															True = case model cart Pay of 
																	(pl, [val]) = (pl, [val - (euro p)])
															False = model cart Pay 

//Start = ["pValueAfterRem : " : test pValueAfterRem]
// ["pValueAfterRem : ","Passed"]
    
pFairRem :: [Product] Product -> Bool
pFairRem listP p 
  | isMember p listP = euro (snd (model remainingList Pay)) == euro (snd (model (delete p listP) Pay))  
  | otherwise        = euro (snd (model remainingList Pay)) == euro (snd (model listP Pay))
  where
      (remainingList, remainingEuro) = model listP (Rem p)
      
Start = ["pFairRem : " : test pFairRem]
// ["pFairRem : ","Passed"]