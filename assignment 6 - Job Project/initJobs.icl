// Brigel Pineti s1005549
// Tim Turksema s1013838

module initJobs

import iTasks
import StdEnv
import StdGeneric
import StdMaybe
import Data.List
	
:: Job = 
    { jobName      :: String
    , skillsNeeded :: [Skill]
    }
    
:: Skill = Java | C | Python | Javascript
    
:: Worker :== (String, [Skill])

instance == (Skill) where
    == Java Java             = True
    == C C                   = True
    == Python Python         = True
    == Javascript Javascript = True
    == _ _                   = False
        
instance == Job where
	== jobA jobB = jobA.jobName == jobB.jobName
	
instance == Worker where
    == (nameA, _) (nameB, _) = nameA == nameB

derive class iTask Skill, Job

Start :: *World -> *World
Start world = doTasks taskLogin world
//Start = matchSkillsToJob [Javascript, Python] jobs  

jobs :: [Job]
jobs  =
	[{jobName      = "Front-end"
	 ,skillsNeeded = [Javascript]
	 }
	 ,
	 {jobName      = "Machine Learning"
	 ,skillsNeeded = [Python]
	 }
	 ,
	 {jobName      = "Embedded"
	 ,skillsNeeded = [Java, C, Python]
	 }
	]
	
sharedJobs :: Shared [Job]
sharedJobs = sharedStore "Available Jobs" jobs

taskEnterSkills :: Task [Skill]
taskEnterSkills = enterMultipleChoice "What are your skills?" [ChooseFromCheckGroup id] [Java, C, Python, Javascript]

taskEnterName :: Task String
taskEnterName = enterInformation "Enter your name" []  

taskWorker :: Task Worker
taskWorker = taskEnterName -&&- taskEnterSkills  

taskLogin :: Task [Job]
taskLogin = taskWorker >>* [OnAction (Action "Login") (hasValue (\worker -> workerActions worker sharedJobs))]           

workerActions :: Worker (Shared [Job]) -> Task [Job]
workerActions (name, skills) sharedJ = viewInformation "Welcome back" [] (" " +++ name, skills)
                                   ||- (filterJobs skills sharedJ 
                                   >>= \filteredJobs -> enterChoiceWithShared "Choose job to complete" [ChooseFromGrid id] (sharedStore "Available Jobs" filteredJobs))
                                   >>* [ OnAction (Action "Create") (always (createNewJob (name, skills) sharedJ))
                                       , OnAction (Action "Edit") (always (editSkills (name, skills) sharedJ))
                                       , OnAction (Action "Execute") (hasValue (executeJob (name, skills) sharedJ))
                                       , OnAction (Action "Cancel") (hasValue (\job -> workerActions (name, skills) sharedJ))
                                       ]
                                       
executeJob :: Worker (Shared [Job]) Job -> Task [Job]
executeJob worker sharedJ job = upd (\jobs -> delete job jobs) sharedJ
                            >>= \_ -> workerActions worker sharedJ

filterJobs :: [Skill] (Shared [Job]) -> Task [Job]
filterJobs mySkills sharedJ = upd (\jobs -> (matchSkillsToJob mySkills jobs)) sharedJ

matchSkillsToJob :: [Skill] [Job] -> [Job]
matchSkillsToJob [] []           = []
matchSkillsToJob [] jobs         = []
matchSkillsToJob mySkills []     = []
matchSkillsToJob mySkills [j:js]
  | belongs j.Job.skillsNeeded mySkills = [j] ++ (matchSkillsToJob mySkills js)
                                        = matchSkillsToJob mySkills js

belongs :: [Skill] [Skill] -> Bool
belongs [] _              = True
belongs neededSkills []   = False
belongs [ns:nss] mySkills = (elem ns mySkills) && (belongs nss mySkills) 
                                                     
createNewJob :: Worker (Shared [Job]) -> Task [Job]
createNewJob worker sharedJ = enterInformation "Enter the name of the job" [] 
                          >>= \name -> enterMultipleChoice "Enter the skills required for the job" [ChooseFromCheckGroup id] [Java, C, Python, Javascript]
                          >>= \skills -> upd (\jobs -> appendJob {jobName = name, skillsNeeded = skills} jobs) sharedJ
                          >>= \_ -> workerActions worker sharedJ
                
appendJob :: Job [Job] -> [Job]
appendJob job [] = [job]
appendJob job list = [job:list] 
                           

editSkills :: Worker (Shared [Job]) -> Task [Job]
editSkills (name, skills) sharedJ = viewInformation (name +++ " Personal Information") [] (name, skills)
                        ||- updateInformation "Update set of skills" [updater] (name, skills)
                        >>* [ OnAction (Action "Save") (hasValue (\worker -> workerActions worker sharedJ))
                            , OnAction (Action "Cancel") (always (workerActions (name, skills) sharedJ)) 
                            ]
                        where 
                            updater = UpdateAs (\(name, skills) -> skills) (\(name, skills) newSkills -> (name, newSkills))   

//checkDuplicateSkills :: Worker -> Bool
//checkDuplicateSkills (_, skills) = if 
