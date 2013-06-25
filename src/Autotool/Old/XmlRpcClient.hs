------------------------------------------------------------------------------
-- | Old (original) interface to simplify communication with the autotool
-- server. Has been renewed (see Autotool.XmlRpc) but still kept as some
-- functions might still be useful.
--
module Autotool.Old.XmlRpcClient
  ( TaskName
  , CategorizedTask (..)
  , Signature (..)
  , Config
  , Documentation
  , TaskDescription
  , ErrorDescription
  , getTaskNames
  , getInitialTaskConfig
  , verifyTaskConfig
  , createTaskInstance
  , evaluateTaskSolution
  , getScoringOrder
  ) where


------------------------------------------------------------------------------
-- IMPORTS                                                                  --
------------------------------------------------------------------------------

import           Control.Applicative ((<$>))

import qualified Autotool.XmlRpc.Interface.Direct      as ID

import qualified Autotool.XmlRpc.Types.Basic           as B
import qualified Autotool.XmlRpc.Types.Documented      as D
import qualified Autotool.XmlRpc.Types.Instance        as I
import qualified Autotool.XmlRpc.Types.Signed          as S
import qualified Autotool.XmlRpc.Types.TaskDescription as TD
import qualified Autotool.XmlRpc.Types.TaskTree        as TT

import           Autotool.XmlRpc.Types.ScoringOrder (ScoringOrder)


------------------------------------------------------------------------------
-- DATA TYPES                                                               --
------------------------------------------------------------------------------

type CodeString = String
type HashString = String
type TextString = String
type XmlString  = String

------------------------------------------------------------------------------

type TaskName = String
type Category = String
type Seed     = String

type Score = Double

type Config   = CodeString
type Solution = CodeString

type TaskDescription = TextString

type Documentation         = XmlString
type ErrorDescription      = XmlString
type EvaluationDescription = XmlString

------------------------------------------------------------------------------
-- | Used to list items from a tree structure without loosing their context.
--   This makes it possible
data CategorizedTask = Categorized
  { taskName :: TaskName
  , context  :: [Category]  -- ^ hierarchical list of categories
  } deriving (Show)

data Signature = Signature
  { sigName :: TaskName
  , body    :: Config
  , key     :: HashString
  } deriving (Show)


------------------------------------------------------------------------------
-- INTERFACE                                                                --
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- | This function makes it easier to list and access single task names but
--   keeps the context in which the task was categorized into.
getTaskNames :: IO [CategorizedTask]
getTaskNames =
    (concatMap $ construct []) <$> ID.getTaskTypes
  where
    construct :: [B.Name] -> TT.TaskTree -> [CategorizedTask]
    construct cs (TT.Category c s) = concatMap (construct $ c:cs) s
    construct cs (TT.Task     n)   = [Categorized n cs]


------------------------------------------------------------------------------
-- | Renamed version of getTaskDescription as acutally no description is
--   returned but an inital task configuration with some documentation links.
getInitialTaskConfig :: TaskName -> IO (Config, Documentation)
getInitialTaskConfig name = do
    td <- TD.taskSampleConfig <$> ID.getTaskDescription name
    return (D.contents td, D.documentation td)


------------------------------------------------------------------------------
-- | Loosed the Signature from an ADT into a single value to make it easier
--   accessible. The server formatted config string is packed into the
--   signature together with the task name now as it will always only be
--   usable in that context.
verifyTaskConfig :: TaskName
                 -> Config
                 -> IO (Either ErrorDescription Signature)
verifyTaskConfig taskname config = do
    v <- ID.verifyTaskConfig taskname config
    either (return . Left) (return . Right . packSignature) v
  where
    packSignature :: S.Signed (B.Task, B.Config) -> Signature
    packSignature (S.Signed (name, cfg) sig) = Signature name cfg sig


------------------------------------------------------------------------------
-- | Renamed to create as it is more interactive related to what is passed
--   into the function. Also the return touple is slightly loosed from ADTs as
--   above.
--
--   The returned solution is an example that will probably evaluate to false
--   but shows how the solution has to be formatted.
createTaskInstance :: Signature
                   -> Seed
                   -> IO (Signature, TaskDescription, Solution, Documentation)
createTaskInstance (Signature n c1 k1) seed = do
    (sig, desc, sol) <- ID.getTaskInstance (S.Signed (n, c1) k1) seed
    let (S.Signed     (_, I.Instance _ c2) k2)  = sig
        (D.Documented ex                   doc) = sol
    return (Signature n c2 k2, desc, ex, doc)


------------------------------------------------------------------------------
-- | Evaluates a submitted solution for a certains task instance (name +
--   config). Returns the (textual) description of the evaluation process as
--   well as the score if the solution did solve the task.
evaluateTaskSolution :: Signature
                     -> Solution
                     -> IO (EvaluationDescription, Maybe Score)
evaluateTaskSolution (Signature n c k) solution = do
    result <- ID.gradeTaskSolution (S.Signed (n, I.Instance n c) k) solution
    case result of
      Left  desc                      -> return (desc, Nothing)
      Right (D.Documented score desc) -> return (desc, Just score)


------------------------------------------------------------------------------
-- | New function to get the scoring order only as this is now removed from
--   the original getTaskDescription function.
getScoringOrder :: TaskName -> IO ScoringOrder
getScoringOrder name = do
    (TD.TaskDescription _ so) <- ID.getTaskDescription name
    return so
