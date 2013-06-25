------------------------------------------------------------------------------
-- | This module offers an interface to access the autotool xml rpc server as
--   defines in the autOlat protocol 0.1.0.
--
module Autotool.XmlRpc.Interface.Direct
  ( TaskInstance
  , getServerInfo
  , getTaskTypes
  , getTaskDescription
  , verifyTaskConfig
  , getTaskInstance
  , gradeTaskSolution
  ) where

------------------------------------------------------------------------------
import Network.XmlRpc.Client                 (remote)
------------------------------------------------------------------------------
import Autotool.XmlRpc.Types.Basic           (Config, Description, Seed,
                                              Solution, Task)
import Autotool.XmlRpc.Types.Documented      (Documented)
import Autotool.XmlRpc.Types.Either          ()
import Autotool.XmlRpc.Types.Instance        (Instance)
import Autotool.XmlRpc.Types.ServerInfo      (ServerInfo)
import Autotool.XmlRpc.Types.Signed          (Signed)
import Autotool.XmlRpc.Types.TaskDescription (TaskDescription)
import Autotool.XmlRpc.Types.TaskTree        (TaskTree)


------------------------------------------------------------------------------
-- | Some aliases for convenience.
type TaskInstance = (Signed (Task,Instance), Description, Documented Solution)


------------------------------------------------------------------------------
-- | The URL of the autotool backend server.
serverUrl :: String
serverUrl = "http://autolat.imn.htwk-leipzig.de/cgi-bin/autotool-0.2.1.cgi"


------------------------------------------------------------------------------
-- | Get the server version info.
getServerInfo :: IO ServerInfo
getServerInfo = remote serverUrl "get_server_info"


------------------------------------------------------------------------------
-- | Get all available task types from the server.
getTaskTypes :: IO [TaskTree]
getTaskTypes = remote serverUrl "get_task_types"


------------------------------------------------------------------------------
-- | Get the specific task description for a given task.
getTaskDescription :: Task -> IO TaskDescription
getTaskDescription = remote serverUrl "get_task_description"


------------------------------------------------------------------------------
-- | Verify the config for a given task to receive a signature.
verifyTaskConfig :: Task 
                 -> Config 
                 -> IO ( Either 
                           Description 
                           (Signed (Task, Config))
                       )
verifyTaskConfig = remote serverUrl "verify_task_config"


------------------------------------------------------------------------------
-- | Get an instance of a verified task.
getTaskInstance :: Signed (Task, Config) 
                -> Seed 
                -> IO ( Signed (Task, Instance)
                      , Description
                      , Documented Solution
                      )
getTaskInstance = remote serverUrl "get_task_instance"

------------------------------------------------------------------------------
-- | Grade a possible solution for a specific task and return the resulting
--   score if correct or a description if wrong.
gradeTaskSolution :: Signed (Task, Instance)
                  -> Solution 
                  -> IO ( Either
                            Description
                            (Documented Double)
                        )
gradeTaskSolution = remote serverUrl "grade_task_solution"
