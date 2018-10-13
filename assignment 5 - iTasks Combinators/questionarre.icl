// Brigel Pineti s1005549
// Tim Turksema s1013838

module questionarre

import iTasks, Data.List, StdEnv, Text

:: Function = Student | Teacher | Admin

:: Question =
	{
	question :: String
	,answers :: [String]
	,correct :: Int
	}

functions :: [Function]
functions = 
	[
	Student
	,
	Teacher
	,
	Admin
	]

questions :: [Question]
questions = [
			{
			question = "What's 1+1", 
			answers = ["1", "2", "3"], 
			correct = 1
			},
			{
			question = "2+2",
			answers = ["4","5","6"],
			correct = 0
			}]

derive class iTask Question, Function

login :: [Function] -> Task [Question]
login functions = enterChoice "What is your role?" [ChooseFromGrid id] functions
	>>= \choice -> viewQuestions choice

viewQuestions :: Function -> Task [Question]
viewQuestions Admin = updateInformation "Questions" [] questions
	>>* [OnAction ActionCancel (always (login functions))]
viewQuestions Student = (answerQuestions questions 0)
viewQuestions Teacher = editQuestionsTeacher

answerQuestions:: [Question] Int -> Task [Question]
answerQuestions [x:r] n = (enterChoice (x.question) [ChooseFromDropdown id] x.answers)
		>>= \choice -> (answerQuestions r (n+(checkAnswer x.answers choice x.correct)))
answerQuestions [] n = (viewInformation "Your results" [ViewAs (\n -> toString n)] n)
		>>* [OnAction (Action "Quit")(always(login functions))]

checkAnswer:: [String] String Int -> Int
checkAnswer answers choice 0 = case choice == (head answers) of
	True -> 1
	_ ->  0
checkAnswer answers choice n = checkAnswer (tail answers) choice (n-1)

editQuestionsTeacher:: Task [Question]
editQuestionsTeacher = updateInformation "Questions" [] questions
	>>* [OnAction ActionCancel (always (login functions))]

listView:: Task [Question]
listView = enterInformation "Edit list of question" []

Start world = startEngine (login functions) world