module Autotool.XmlRpc.Interface.CommandLine
    ( main
    , pickTaskName
    , showTaskTemplateInfo
    , enterTaskConfig
    ) where

import           Autotool.XmlRpc.Types.TaskTree (TaskTree (Task, Category))
import           Autotool.XmlRpc.Types.ScoringOrder (ScoringOrder)
import           Autotool.XmlRpc (
    getTaskNames          -- #1 |
  , getInitialTaskConfig  -- #2 | Tutor
  , submitTaskConfig      -- #3 | 
  , getTaskInstance       -- #4   | Student
  , submitSolution        -- #5   |
  )

pickTaskName :: IO String
pickTaskName = do
  taskTrees <- getTaskNames
  showTaskTrees taskTrees
  putStrLn "--------------------------------------"
  putStrLn "pick a task name:"
  getLine

showTaskTemplateInfo :: String -> [(String, String)] -> ScoringOrder -> IO ()
showTaskTemplateInfo sampleConfig configDoc scoringOrder = do
  putStrLn ""
  putStrLn ("scoring order: " ++ show scoringOrder)
  putStrLn ""
  putStrLn "documentation for config types:"
  mapM_ (putStrLn . show) configDoc
  --putStrLn configDoc
  putStrLn ""
  putStrLn "example configuration:"
  putStrLn sampleConfig
  putStrLn ""

enterTaskConfig :: String -> String -> IO String
enterTaskConfig taskName sampleConfig = do
  putStrLn "--------------------------------------"
  putStrLn "enter configuration:"

  enterConfigUntilValid taskName sampleConfig

-----------------------------------------------------------------------------
-- | Main routine. All the stuff happens here.
main :: IO ()
main = do
  putStrLn "---------------------------------------------" 
  putStrLn "Autotool XmlRpc Client Command Line Interface" 
  putStrLn "---------------------------------------------" 
  putStrLn ""
  
  taskName <- pickTaskName
  (sampleConfig, configDoc, scoringOrder) <- getInitialTaskConfig taskName

  showTaskTemplateInfo sampleConfig configDoc scoringOrder

  configSig <- enterTaskConfig taskName sampleConfig

  putStrLn "Validation successful! Your signature is: "
  putStrLn configSig
  putStrLn "--------------------------------------"
  putStrLn "enter a seed to create a task instance:"

  seed <- getLine
  (desc, sampleSolution, solutionDoc, taskSig) <- getTaskInstance configSig seed

  putStrLn ""
  putStrLn "Here is your task instance:"
  putStrLn "signature"
  putStrLn taskSig
  putStrLn ""
  putStrLn "task description"
  putStrLn desc
  putStrLn ""
  putStrLn "documentation"
  mapM_ (putStrLn . show) solutionDoc
  putStrLn ""
  putStrLn "example solution"
  putStrLn sampleSolution
  putStrLn "--------------------------------------"
  putStrLn "enter a solution:"

  result <- enterSolutionUntilValid taskSig sampleSolution

  putStrLn "--------------------------------------"
  putStrLn "Nice! Your result is:"
  putStrLn ""
  putStrLn result


-----------------------------------------------------------------------------
-- | Enter a task config until it is verified successfully.
enterConfigUntilValid :: String -> String -> IO String
enterConfigUntilValid taskName sampleConfig = do
  userConfig <- getLines
  let config = if null userConfig
                  then sampleConfig
                  else userConfig
  result <- submitTaskConfig taskName config
  case result of
    (Left err) -> do putStrLn err
                     putStrLn "try again:"
                     enterConfigUntilValid taskName sampleConfig
    (Right sig) -> return sig


-----------------------------------------------------------------------------
-- | Enter a task solution until it is verified successfully.
enterSolutionUntilValid :: String -> String -> IO String
enterSolutionUntilValid sig sampleSolution = do
  userSolution <- getLines
  let solution = if null userSolution
                    then sampleSolution
                    else userSolution
  response <- submitSolution sig solution
  case response of
    (Left err) -> do putStrLn err
                     putStrLn "try again:"
                     enterSolutionUntilValid sig sampleSolution
    (Right result) -> return result


-----------------------------------------------------------------------------
-- | Get multiple lines until blank line.
getLines :: IO String
getLines = do line <- getLine
              if line == ""
                then return line
                else do more <- getLines
                        return $ line ++ more


-----------------------------------------------------------------------------
-- | Print tasktrees in tree structure to console.
showTaskTrees :: [TaskTree] -> IO ()
showTaskTrees = mapM_ $ showTaskTree 0


-----------------------------------------------------------------------------
-- | Print the tree structure of categorized task names to the console.
showTaskTree :: Int -> TaskTree -> IO ()
showTaskTree ident (Task task) = do
  putStr (replicate ident ' ')
  putStrLn task 
showTaskTree ident (Category cat subs) = do
  putStr (replicate ident ' ')
  putStrLn cat
  mapM_ (showTaskTree $ ident + 2) subs
  putStrLn ""
