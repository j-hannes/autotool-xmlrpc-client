------------------------------------------------------------------------------
-- | This module contains the TaskDescription data type.
--
module Autotool.XmlRpc.Types.TaskDescription (
    TaskDescription (..)
  ) where

------------------------------------------------------------------------------
import Network.XmlRpc.Internals
------------------------------------------------------------------------------
import Autotool.XmlRpc.Types.Basic        (Config)
import Autotool.XmlRpc.Types.Documented   (Documented)
import Autotool.XmlRpc.Types.ScoringOrder (ScoringOrder)

------------------------------------------------------------------------------
data TaskDescription = TaskDescription {
    taskSampleConfig :: Documented Config
  , taskScoringOrder :: ScoringOrder
  } deriving Show

instance XmlRpcType TaskDescription where
  toValue s = toValue [("task_sample_config", toValue (taskSampleConfig s)),
                       ("task_scoring_order", toValue (taskScoringOrder s))]
  fromValue v = do t <- fromValue v
                   s <- getField "TaskDescription" t
                   sc <- getField "task_sample_config" s
                   so <- getField "task_scoring_order" s
                   return $ TaskDescription sc so
  getType _ = TStruct
