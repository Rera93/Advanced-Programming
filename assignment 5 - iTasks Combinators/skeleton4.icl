// Brigel Pineti s1005549
// Tim Turksema s1013838

module skeleton4

import iTasks
import StdEnv
import StdGeneric
import StdMaybe
import Data.List

/*
	Pieter Koopman, pieter@cs.ru.nl
	Advanced Programming. Skeleton for assignment 4 in 2018
	use this a project with environment iTasks or use ready to use accompanied
	project file from brightspace
*/

:: Function = Student | Teacher | Admin

instance == (Function) where
    == Student Student = True
    == Teacher Teacher = True
    == Admin Admin     = True
    == _ _             = False

:: Question =
	{ question :: String
	, answers  :: [String]
	, correct  :: Int
	}
	
instance == Question where
	== qa qb = qa.question == qb.question

derive class iTask Function, Login, Question

:: Login = 
    { username :: String
    , function :: Function
    }

Start :: *World -> *World
Start world = doTasks taskQuestion world

questions :: [Question]
questions  =
	[{question = "What year do we live in?"
	 ,answers  = ["2017", "2018", "2019"]
	 ,correct = 1
	 }
	,{question = "Bob who?"
     ,answers  = ["Mraley", "Marlye", "Marley"]
	 ,correct = 2
	 }
	]
	
sharedQuestions :: Shared [Question]
sharedQuestions = sharedStore "Up-to-date questions" questions 
	
taskLogin :: Task Login
taskLogin = enterInformation "Enter username and function " [] 
                
taskQuestion :: Task [Question]
taskQuestion = taskLogin 
    >>* [ OnAction (Action "Login") (ifValue (\({username,function}) -> size username >= 3)(\user -> redirect user sharedQuestions))]
    
redirect :: Login (Shared [Question]) -> Task [Question]
redirect {username, function} shared
    | function == Student = studentTask username shared
    | function == Teacher = teacherTask username shared
    | function == Admin   = adminTask username shared

answerQuestion :: Question -> Task Bool
answerQuestion q = viewInformation "Question: " [] q.Question.question
                 ||- enterChoice "Select the correct answer: " [ChooseFromGrid id] q.Question.answers
                 >>= \answer -> return (elemIndex answer q.Question.answers == Just (q.Question.correct)) 

studentTask :: String (Shared [Question]) -> Task [Question]
studentTask username sharedQ = get sharedQ 
                           >>= \questions -> sequence (map answerQuestion questions)
                           >>= \correctAns -> (viewInformation "Correct answers" [] (calc correctAns)
                           ||- viewInformation "Wrong answers" [] ((length questions) - (calc correctAns)))
                           >>| taskQuestion
                           where
                           calc rightAns = foldr (\r a -> if r (a+1) a) 0 rightAns

adminTask :: String (Shared [Question]) -> Task [Question]
adminTask username sharedQ = viewInformation ("Welcome back, " +++ username) [] ""
                         ||- updateSharedInformation "Updated Questions" [] sharedQ 

teacherTask :: String (Shared [Question]) -> Task [Question]
teacherTask username sharedQ = viewInformation ("Welcome back, " +++ username) [] ""
                           ||- enterChoiceWithShared "Edit by selecting one of the items first" [ChooseFromGrid id] sharedQ
                           >>* [ OnAction (Action "Append") (hasValue (append sharedQ))
		                       , OnAction (Action "Delete") (hasValue (del sharedQ))
		                       , OnAction (Action "Edit") (hasValue (edit username sharedQ))
		                       , OnAction (Action "First") (always (first sharedQ))
		                       , OnAction (Action "Clear") (hasValue (\q -> replace q sharedQ defaultValue))
		                       , OnAction (Action "Quit") (always taskQuestion)
		                       ]
	                       >>= \_ -> teacherTask username sharedQ
	                       
append :: (Shared [Question]) Question -> Task [Question]
append sharedQ question = upd (\questions -> appendList question questions) sharedQ

del :: (Shared [Question]) Question -> Task [Question]
del sharedQ question = upd (\questions -> delete question questions) sharedQ

edit :: String (Shared [Question]) Question -> Task [Question]
edit username sharedQ question = updateInformation "Edit Question" [] question
	                          >>* [ OnAction ActionOk (hasValue add)
		                          , OnAction ActionCancel (always (teacherTask username sharedQ))]
	                         where
		                       add newQ = replace question sharedQ newQ
		                           	>>= \_ -> teacherTask username sharedQ

first :: (Shared [Question]) -> Task [Question]
first sharedQ = upd (\questions -> [defaultValue:questions]) sharedQ

replace :: Question (Shared [Question]) Question -> Task [Question]
replace observedQ sharedQ newQ = upd (\questions -> replaceList observedQ questions newQ) sharedQ

appendList :: a [a] -> [a] | iTask, == a
appendList _ [] = []
appendList q [x:xs]
	| q == x = [x,defaultValue:xs]
	= [x:appendList q xs]

replaceList :: a [a] a -> [a] | == a
replaceList _ [] _ = []
replaceList oq [x:xs] nq 
	| x == oq = [nq:xs]
	= [x:replaceList oq xs nq]

