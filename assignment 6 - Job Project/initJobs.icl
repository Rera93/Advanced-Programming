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

:: WorkerHome :== ((String, [Skill]), [Job])

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

taskEnterSkills :: Task [Skill]
taskEnterSkills = enterMultipleChoice "What are your skills?" [ChooseFromCheckGroup id] [Java, C, Python, Javascript]

taskEnterName :: Task String
taskEnterName = enterInformation "Enter your name" []  

taskWorker :: Task Worker
taskWorker = taskEnterName -&&- taskEnterSkills  

taskLogin :: Task WorkerHome
taskLogin = taskWorker >>* [OnAction (Action "Login") (hasValue (\worker -> workerInterfaceTask worker sharedJobs))]
         
workerInterfaceTask :: Worker (Shared [Job]) -> Task WorkerHome
workerInterfaceTask (name, skills) sharedJ = (updateSkillsTask (name, skills)) -&&- (workerActionsTask (name, skills) sharedJ)
                                       
updateSkillsTask :: Worker -> Task (String, [Skill])
updateSkillsTask (name, skills) = updateInformation ("Welcome back " +++ name +++ "!") [updater] (name, skills)
                              >>* [ OnValue (ifValue hasNoDupSkills (\worker -> return worker)) ]
                              where
                                  updater = UpdateAs (\(name, skills) -> skills) (\(name, skills) newSkills -> (name, newSkills))

workerActionsTask :: Worker (Shared [Job]) -> Task [Job]
workerActionsTask (name, skills) sharedJ = (filterJobs skills sharedJ 
                                       >>= \filteredJobs -> enterChoiceWithShared "Choose job to complete" [ChooseFromGrid id] (sharedStore "Available Jobs" filteredJobs))
                                   >>* [ OnAction (Action "Create") (always (createNewJob (name, skills) sharedJ))
                                       , OnAction (Action "Edit") (always (editSkills (name, skills) sharedJ))
                                       , OnAction (Action "Execute") (ifValue isIndependent (executeJob (name, skills) sharedJ))
                                       , OnAction (Action "ExecuteSub") (ifValue isDependent (executeSubJob (name, skills) sharedJ))
                                       , OnAction (Action "Cancel") (hasValue (\job -> workerActionsTask (name, skills) sharedJ))
                                       , OnAction (Action "Split") (hasValue (splitJob (name, skills) sharedJ))
                                       ]

                                       
splitJob :: Worker (Shared [Job]) Job -> Task [Job]
splitJob worker sharedJ jobToSplit = viewInformation "Split job in sub-jobs" [] jobToSplit
                         // ||- updateSharedInformation "Add/Remove/Edit sub-jobs" [] (sharedStore "" subJobs)
                          ||- addSubJobs jobToSplit 
                          >>= \listOfSubJob -> return (listOfSubJob ++ jobToSplit.Job.subJobs)
                          >>= \listOfSubJobs -> get sharedJ
                          >>= \jobs -> set (appendJobs listOfSubJobs jobs jobToSplit) sharedJ
                          //>>= \listOfSubJobs -> upd (\jobs -> appendJobs listOfSubJobs jobs jobToSplit) sharedJ 
                          >>= \_ -> workerActionsTask worker sharedJ
                         // where
                          //    updater = UpdateAs (\[j] -> [j.Job.jobName]) (\[subJob] newName -> [{[newName, job.Job.skillsNeeded, job.Job.subJobs, Child]}])
                            
          
addSubJobs :: Job -> Task [Job]
addSubJobs job = updateInformation "New name for job" [] job.Job.jobName
                  >>= \newName -> return [{job & jobName = newName, subJobs = [], relation = job.Job.jobName }]  

executeSubJob :: Worker (Shared [Job]) Job -> Task [Job]
executeSubJob worker sharedJ job = //upd (\jobs -> delete job jobs) sharedJ
                                    get sharedJ
                               >>= \jobs -> return (delete job jobs)
                               >>= \newJobs -> set (deleteSubJob job newJobs) sharedJ
                              // >>= \_ -> upd (\jobs -> deleteSubJob job jobs) sharedJ
                               >>= \newSharedJ -> workerActionsTask worker sharedJ                                                                                                

isIndependent :: Job -> Bool 
isIndependent job 
  | (job.Job.subJobs == [] && job.Job.relation == "") = True
                                                      = False 
                                
isDependent :: Job -> Bool
isDependent job
  | (job.Job.relation <> "" && job.Job.subJobs == []) = True
                                                      = False

executeJob :: Worker (Shared [Job]) Job -> Task [Job]
executeJob worker sharedJ job = upd (\jobs -> delete job jobs) sharedJ
                            >>= \_ -> workerActionsTask worker sharedJ
                            
deleteSubJob :: Job [Job] -> [Job]
deleteSubJob job []     = []
deleteSubJob job [j:js]
  | elem job j.Job.subJobs = [{jobName = j.Job.jobName
                              ,skillsNeeded = j.Job.skillsNeeded
                              ,subJobs = delete job j.Job.subJobs
                              ,relation = j.Job.relation 
                              }] ++ js 
                           = [j] ++ deleteSubJob job js

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
                          >>= \name -> enterMultipleChoice "Enter tfor he skills required for the job" [ChooseFromCheckGroup id] [Java, C, Python, Javascript]
                          >>= \skills -> upd (\jobs -> appendJob {jobName = name, skillsNeeded = skills, subJobs = [], relation = ""} jobs) sharedJ
                          >>= \_ -> workerActionsTask worker sharedJ
                
appendJob :: Job [Job] -> [Job]
appendJob job [] = [job]
appendJob job list = [job:list] 

appendJobs :: [Job] [Job] Job -> [Job]
appendJobs [] [] job             = []
appendJobs [] jobs job           = jobs
appendJobs subJobs [j:js] job 
  | j == job                   = [{jobName = j.Job.jobName, skillsNeeded = j.Job.skillsNeeded, subJobs = subJobs, relation = j.Job.relation}] ++ [(head subJobs)] ++ js
                               = [j] ++ (appendJobs subJobs js job)   

                           

editSkills :: Worker (Shared [Job]) -> Task [Job]
editSkills (name, skills) sharedJ = viewInformation (name +++ " Personal Information") [] (name, skills)
                        ||- updateInformation "Update set of skills" [updater] (name, skills)
                        >>* [ OnAction (Action "Save") (ifValue hasNoDupSkills (\worker -> updateJobs worker sharedJ)) //works only when removing,
                            , OnAction (Action "Cancel") (always (workerActionsTask (name, skills) sharedJ)) 
                            ]
                        where 
                            updater = UpdateAs (\(name, skills) -> skills) (\(name, skills) newSkills -> (name, newSkills))
                           
updateJobs :: Worker (Shared [Job]) -> Task [Job]
updateJobs (name, skills) sharedJ = filterJobs skills sharedJ
                                >>= \filteredJobs -> upd (\jobs -> filteredJobs) sharedJ
                                >>= \_ -> workerActionsTask (name, skills) (sharedStore "" filteredJobs) 
                                
hasNoDupSkills :: Worker -> Bool
hasNoDupSkills (name, skills)
  | hasDup skills = False
                  = True