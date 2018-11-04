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
    , subJobs      :: [Job]
    , relation     :: String
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

jobs :: [Job]
jobs  =
	[{jobName      = "Front-end"
	 ,skillsNeeded = [Javascript]
	 ,subJobs      = []
	 ,relation     = ""
	 }
	 ,
	 {jobName      = "Machine Learning"
	 ,skillsNeeded = [Python]
	 ,subJobs      = []
	 ,relation     = ""
	 }
	 ,
	 {jobName      = "Embedded"
	 ,skillsNeeded = [Java, C, Python]
	 ,subJobs      = []
	 ,relation     = ""
	 }
	]
	
subJobs :: [Job]
subJobs = []

sharedJobs :: Shared [Job]
sharedJobs = sharedStore "Available Jobs" jobs
	
shareStore :: Worker -> Shared (Worker, [Job])
shareStore worker = sharedStore "Available (Worker, Jobs)" (worker, jobs)

taskEnterSkills :: Task [Skill]
taskEnterSkills = enterMultipleChoice "What are your skills?" [ChooseFromCheckGroup id] [Java, C, Python, Javascript]

taskEnterName :: Task String
taskEnterName = enterInformation "Enter your name" []  

taskWorker :: Task Worker
taskWorker = taskEnterName -&&- taskEnterSkills  

taskLogin :: Task [Job]
taskLogin = taskWorker >>* [OnAction (Action "Login") (hasValue (\worker -> forever (workerRefreshTask worker)))]
         
workerRefreshTask :: Worker -> Task [Job] 
workerRefreshTask worker = get sharedJobs 
                         >>= \shjobs -> upd (\(w, jobss) -> (worker, shjobs )) (shareStore worker) 
                         >>= \_ -> workerActionsTask (shareStore worker) sharedJobs
                                       
updateSkillsTask :: Worker -> Task (String, [Skill])
updateSkillsTask (name, skills) = updateInformation ("Welcome back " +++ name +++ "!") [updater] (name, skills)
                              >>* [ OnValue (ifValue hasNoDupSkills (\worker -> return worker)) ]
                              where
                                  updater = UpdateAs (\(name, skills) -> skills) (\(name, skills) newSkills -> (name, newSkills))


workerActionsTask :: (Shared (Worker, [Job])) (Shared [Job]) -> Task [Job]
workerActionsTask sharedJ sharedJobs = get sharedJ
                        >>= \((name, skills), jobs) -> viewInformation "Welcome back" [] (name, skills)
                        ||- (filterJobs skills sharedJ
                        >>= \_ -> get sharedJ 
                        >>= \(w, filteredJobs) -> set jobs sharedJobs
                        >>= \_ -> enterChoiceWithShared "Choose job to complete" [ChooseFromGrid id] (sharedStore "Filtered Jobs" filteredJobs))
                                   >>* [ OnAction (Action "Create") (always (createNewJob sharedJ sharedJobs))
                                       , OnAction (Action "Edit") (always (editSkills sharedJ sharedJobs))
                                       , OnAction (Action "Execute") (ifValue isIndependent (executeJob sharedJ sharedJobs))
                                       , OnAction (Action "ExecuteSub") (ifValue isDependent (executeSubJob sharedJ sharedJobs))
                                       , OnAction (Action "Cancel") (hasValue (\job -> workerActionsTask sharedJ sharedJobs))
                                       , OnAction (Action "Split") (hasValue (splitJob sharedJ sharedJobs))
                                       ]

                                       
splitJob :: (Shared (Worker,[Job])) (Shared [Job]) Job -> Task [Job]
splitJob sharedJ sharedJobs jobToSplit = viewInformation "Split job in sub-jobs" [] jobToSplit
                                 ||- addSubJobs jobToSplit sharedJ
                                 >>= \listOfSubJob -> return (listOfSubJob ++ jobToSplit.Job.subJobs)
                                 >>= \listOfSubJobs -> get sharedJ
                                 >>= \(w, jobs) -> set (w, (appendJobs listOfSubJobs jobs jobToSplit)) sharedJ
                                 >>= \_ -> workerActionsTask sharedJ sharedJobs
          
addSubJobs :: Job (Shared (Worker, [Job]))-> Task [Job]
addSubJobs job sharedJ = get sharedJ
             >>= \(w, jobs) -> updateInformation "New name for job" [] job.Job.jobName
             >>* [ OnAction (Action "Add") (ifValue (hasNoDupNames jobs) (\newName -> return [{job & jobName = newName, subJobs = [], relation = job.Job.jobName }]))] 

executeSubJob :: (Shared (Worker, [Job])) (Shared [Job]) Job -> Task [Job]
executeSubJob sharedJ sharedJobs job = get sharedJ
                               >>= \(w, jobs) -> return (delete job jobs)
                               >>= \newJobs -> set (w, (deleteSubJob job newJobs)) sharedJ
                               >>= \newSharedJ -> workerActionsTask sharedJ sharedJobs                                                                                              

isIndependent :: Job -> Bool 
isIndependent job 
  | (job.Job.subJobs == [] && job.Job.relation == "") = True
                                                      = False 
                                
isDependent :: Job -> Bool
isDependent job
  | (job.Job.relation <> "" && job.Job.subJobs == []) = True
                                                      = False

executeJob :: (Shared (Worker, [Job])) (Shared [Job]) Job -> Task [Job]
executeJob sharedJ sharedJobs job = upd (\(w, jobs) -> (w, (delete job jobs))) sharedJ
                            >>= \_ -> workerActionsTask sharedJ sharedJobs
                            
deleteSubJob :: Job [Job] -> [Job]
deleteSubJob job []     = []
deleteSubJob job [j:js]
  | elem job j.Job.subJobs = [{jobName = j.Job.jobName
                              ,skillsNeeded = j.Job.skillsNeeded
                              ,subJobs = delete job j.Job.subJobs
                              ,relation = j.Job.relation 
                              }] ++ js 
                           = [j] ++ deleteSubJob job js

filterJobs :: [Skill] (Shared (Worker, [Job])) -> Task (Worker, [Job])
filterJobs mySkills sharedJ = upd (\((name, skills), jobs) -> ((name, mySkills), (matchSkillsToJob mySkills jobs))) sharedJ

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
                                                     
createNewJob :: (Shared (Worker, [Job])) (Shared [Job])-> Task [Job]
createNewJob sharedJ sharedJobs = enterInformation "Enter the name of the job" [] 
                          >>* [OnAction (Action "Create") (ifValue (hasNoDupNames jobs) (\name -> return name))]
                          >>= \name -> enterMultipleChoice "Enter the skills required for the job" [ChooseFromCheckGroup id] [Java, C, Python, Javascript]
                          >>= \skills -> upd (\(w, jobs) -> (w, (appendJob {jobName = name, skillsNeeded = skills, subJobs = [], relation = ""} jobs))) sharedJ
                          >>= \_ -> workerActionsTask sharedJ sharedJobs
                
appendJob :: Job [Job] -> [Job]
appendJob job [] = [job]
appendJob job list = [job:list] 

appendJobs :: [Job] [Job] Job -> [Job]
appendJobs [] [] job             = []
appendJobs [] jobs job           = jobs
appendJobs subJobs [j:js] job 
  | j == job                   = [{jobName = j.Job.jobName, skillsNeeded = j.Job.skillsNeeded, subJobs = subJobs, relation = j.Job.relation}] ++ [(head subJobs)] ++ js
                               = [j] ++ (appendJobs subJobs js job)   

                           

editSkills :: (Shared (Worker, [Job])) (Shared [Job]) -> Task [Job]
editSkills sharedJ sharedJobs = get sharedJ 
                 >>= \((name, skills), jobs) -> viewInformation (name +++ " Personal Information") [] (name, skills)
                        ||- updateInformation "Update set of skills" [updater] (name, skills)
                        >>* [ OnAction (Action "Save") (ifValue hasNoDupSkills (\worker -> workerRefreshTask worker)) //works only when removing,
                            , OnAction (Action "Cancel") (always (workerActionsTask sharedJ sharedJobs)) 
                            ]
                        where 
                            updater = UpdateAs (\(name, skills) -> skills) (\(name, skills) newSkills -> (name, newSkills))
                           
updateJobs :: Worker (Shared (Worker, [Job])) (Shared [Job]) -> Task [Job]
updateJobs (name, newSkills) sharedJ sharedJobs = upd (\((name, skills), jobs) -> ((name,newSkills), jobs)) sharedJ
                                  // >>= \_ -> filterJobs newSkills sharedJ
                                   >>= \_ -> workerActionsTask sharedJ sharedJobs
                                
hasNoDupSkills :: Worker -> Bool
hasNoDupSkills (name, skills)
  | hasDup skills = False
                  = True
                  
hasNoDupNames :: [Job] String -> Bool
hasNoDupNames [] _ = True
hasNoDupNames [j:js] name 
  | j.Job.jobName == name = False
                          = hasNoDupNames js name