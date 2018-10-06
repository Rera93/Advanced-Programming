// Brigel Pineti s1005549
// Tim Turksema s1013838

module skeleton4

import iTasks, Data.List

/*
	Pieter Koopman, pieter@cs.ru.nl
	Advanced Programming. Skeleton for assignment 4 in 2018
 -	use this a project with environment iTasks
 -	executable must be in Examples/iTasks or a subdirectory
	You can also use the -sdk commandline flag to set the path
*/

:: Student =
	{ name :: String
	, snum :: Int
	, bama :: BaMa
	, year :: Int
	}

:: BaMa = Bachelor | Master

students :: [Student]
students =
	[{name = "Alice"
	 ,snum = 1000
	 ,bama = Bachelor
	 ,year = 1
	 }
	,{name = "Bob"
	 ,snum = 1003
	 ,bama = Master
	 ,year = 1
	 }
	,{name = "Carol"
	 ,snum = 1024
	 ,bama = Master
	 ,year = 2
	 }
	,{name = "Dave"
	 ,snum = 2048
	 ,bama = Master
	 ,year = 1
	 }
	,{name = "Eve"
	 ,snum = 4096
	 ,bama = Master
	 ,year = 1
	 }
	,{name = "Frank"
	 ,snum = 1023
	 ,bama = Master
	 ,year = 1
	 }
	]

derive class iTask Student, BaMa

//1
addStudent:: Task Student
addStudent = enterInformation "Add a new student" []
//2
addStudents:: Task [Student]
addStudents = enterInformation "Add a list of new students" []
//3
updateStudentInfo:: Student -> Task Student
updateStudentInfo student = updateInformation "Update student information" [] student
//4
pickFavourite :: [Student] -> Task Student
pickFavourite students = enterChoice "Choose your favourite student" [ChooseFromGrid id] students
//5
pickFavouriteNameOnly :: [Student] -> Task Student
pickFavouriteNameOnly students = enterChoice "Choose your favourite student based on name" [ChooseFromGrid (\({name,snum,bama,year}) -> name)] students
//6
gPickFavourite :: [Student] -> Task Student
gPickFavourite students = enterChoice "Choose your favourite student, but generic!" [ChooseFromGrid gToString{|*|}] students
//7
pickPartners :: [Student] -> Task [Student]
pickPartners students = enterMultipleChoice "Choose partners" [ChooseFromGrid (\({name,snum,bama,year})->"Student: " + name + ", " + gToString{|*|} bama)] students
//8


Start :: *World -> *World
Start world = startEngine (updateStudentInfo (head (tail (tail students)))) world

student :: Student
student = students !! 0


generic gToString a :: a -> String
gToString{|Student|} {name,snum,bama,year} = "Student: " + name + ", " + toString snum + " , " + gToString{|*|} bama+ ", " + toString year
gToString{|BaMa|} Bachelor = "Bachelor"
gToString{|BaMa|} Master = "Master"
instance + String where + s t = s +++ t
