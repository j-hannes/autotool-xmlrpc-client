------------------------------------------------------------------------------
-- | This module offers a simplified communication interface to the autotool
--   backend server.
--
module Autotool.XmlRpc
  ( getTaskNames
  , getInitialTaskConfig
  , submitTaskConfig
  , getTaskInstance
  , submitSolution
  ) where


------------------------------------------------------------------------------
-- IMPORTS                                                                  --
------------------------------------------------------------------------------

import           Control.Applicative ((<$>))

import qualified Autotool.XmlRpc.Interface.Direct as Autotool
import           Autotool.XmlRpc.Types.Basic (Task, Config, Solution)
import qualified Autotool.XmlRpc.Types.Documented as D
import           Autotool.XmlRpc.Types.ScoringOrder (ScoringOrder)
import           Autotool.XmlRpc.Types.TaskDescription (TaskDescription (..))
import           Autotool.XmlRpc.Types.TaskTree

import           Autotool.Utils.Encryption
import           Autotool.Utils.XmlParser (parseDocumentation, getNodeContent,
                                           fixSpecialChars)


------------------------------------------------------------------------------
-- DATA TYPES                                                               --
------------------------------------------------------------------------------

type TaskName   = Task
type TaskConfig = Config

type LinkMap = [(String, String)]


------------------------------------------------------------------------------
-- INTERFACE                                                                --
------------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- | Simple export of renamed function.
getTaskNames :: IO [TaskTree]
getTaskNames = Autotool.getTaskTypes

-----------------------------------------------------------------------------
-- | Wrapper to get the configuration details for a specific task.
getInitialTaskConfig :: TaskName -> IO (TaskConfig, LinkMap, ScoringOrder)
getInitialTaskConfig tn = convert <$> Autotool.getTaskDescription tn
  where
    convert td = (config, doc, order)
      where
        order  = taskScoringOrder td
        config = fixSpecialChars . D.contents $ taskSampleConfig td
        doc    = parseDocumentation . D.documentation $ taskSampleConfig td


-----------------------------------------------------------------------------
-- | Wrapper to format XML error messages or encrypt the signature.
submitTaskConfig :: TaskName -> TaskConfig -> IO (Either String String)
submitTaskConfig tn cfg = do
    result <- Autotool.verifyTaskConfig tn cfg
    case result of
      (Left err)  -> return . Left $ getNodeContent err
      (Right sig) -> return . Right . encrypt $ show sig


-----------------------------------------------------------------------------
-- | Wrapper to get the a instance of a configured task.
getTaskInstance :: String -> String -> IO (String, Solution, LinkMap, String)
getTaskInstance encsig seed = convert <$> Autotool.getTaskInstance decsig seed
  where
    decsig = read $ decrypt encsig
    convert (atsig, atdesc, docsol) = (desc, solution, doc, sig)
      where
        sig      = encrypt $ show atsig
        doc      = parseDocumentation $ D.documentation docsol
        solution = D.contents docsol
        desc     = getNodeContent atdesc


-----------------------------------------------------------------------------
-- | Wrapper to format XML error messages or encrypt the signature.
submitSolution :: String -> String -> IO (Either String String)
submitSolution sig solution = do
    result <- Autotool.gradeTaskSolution (read $ decrypt sig) solution
    case result of
      (Left err) -> return . Left $ getNodeContent err
      (Right res) -> return . Right . getNodeContent $ D.documentation res
