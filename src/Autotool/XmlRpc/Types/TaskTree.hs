------------------------------------------------------------------------------
-- | This module contains the TaskTree data type.
--
module Autotool.XmlRpc.Types.TaskTree (
    TaskTree (..)
  ) where

------------------------------------------------------------------------------
import Network.XmlRpc.Internals

import Autotool.XmlRpc.Types.Basic (Name, Task)

------------------------------------------------------------------------------
data TaskTree = Task { taskName :: Task
                     }
              | Category { categoryName :: Name
                         , subTrees :: [TaskTree]
                         } deriving (Read, Show)

instance XmlRpcType TaskTree where
  toValue (Task tn)        = toValue [("task_name",     toValue tn)]
  toValue (Category cn st) = toValue [("category_name", toValue cn),
                                      ("sub_trees",     toValue st)]
  fromValue t = do value <- fromValue t
                   task <- getFieldMaybe "Task" value
                   case task of
                     (Just x) -> do 
                       r <- getField "task_name" x
                       return $ Task (convertUmlautToHtml r)
                     Nothing -> do
                       p <- getField "Category" value
                       n <- getField "category_name" p
                       s <- getField "sub_trees" p
                       return $ Category (convertUmlautToHtml n) s
  getType _ = TStruct


------------------------------------------------------------------------------
-- | Converts a string with escaped umlaut characters into html conform
--   escaped umlaut characters. For example '\228' gets converted to '&#228;'.
--
--   Note the output string might be longer than the input string.
convertUmlautToHtml :: String -> String
convertUmlautToHtml = foldr ((++) . replaceUmlaut) []

replaceUmlaut :: Char -> String
replaceUmlaut '\228' = "ae"
replaceUmlaut '\246' = "oe"
replaceUmlaut '\252' = "ue"
replaceUmlaut x      = [x]
