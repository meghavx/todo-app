{-# LANGUAGE RecordWildCards #-}

module LibIO (cmdMap, fetchArgs) where

import System.IO
import Data.List
import System.Directory
import Options.Applicative (execParser)

import Parsers (progParser)
import Types (Args(..), Action)

fetchArgs :: IO Args
fetchArgs = execParser progParser

cmdMap :: [(String, Action)]
cmdMap =  [("add",  add)
          ,("view", view)
          ,("bump", bump)
          ,("move", move)
          ,("update", update)
          ,("remove", remove)]

add :: Action
add Args {..} = do
  appendFile file $ "– " ++ task ++ "\n"
  putStrLn $ "Task added to the list."

view :: Action
view Args {..} = do
  contents <- readFile file
  let todoTasks = lines contents
  case length todoTasks of
    0 -> putStrLn "Turns out your TODO list is empty!"
    _ -> do
      displayTodoHeader
      let numberedTasks = zipWith prependSNos [1 :: Int ..] todoTasks
          prependSNos n item = " " <> show n <> " " <> item
      putStrLn $ unlines numberedTasks

bump :: Action
bump Args {..} = do
  contents <- readFile file
  let todoTasks = lines contents
      tasksCount = length todoTasks
  (case validIndices index1 index2 False tasksCount of
    Left err -> putStrLn err
    Right _ -> do
      let bumpedTask = todoTasks !! (index1 - 1)
          newTodoTasks = bumpedTask : delete bumpedTask todoTasks
      updateChangesToFile file newTodoTasks
      putStrLn $ "Task #" <> show index1 <> " bumped to top of the list.")

move :: Action
move Args {..} = do
  contents <- readFile file
  let todoTasks = lines contents
      tasksCount = length todoTasks
  case validIndices index1 index2 True tasksCount of
    Left e -> putStrLn e
    Right _ -> do
      if index1 == index2
        then putStrLn "No change made; the values provided for i & j are equal."
        else do
          let taskToMove = todoTasks !! (index1 - 1)
              otherTasks = delete taskToMove todoTasks
              newTodoTasks = take (index2 - 1) otherTasks 
                          <> [taskToMove]
                          <> drop (index2 - 1) otherTasks
          updateChangesToFile file newTodoTasks
          putStrLn $ "Task #" <> show index1 <> " moved to #" <> show index2 <> "."

update :: Action
update Args {..} = do
  contents <- readFile file
  let todoTasks = lines contents
      tasksCount = length todoTasks
  case validIndices index1 index2 False tasksCount of
    Left e -> putStrLn e
    Right _ -> do
      let newTodoTasks = take (index1 - 1) todoTasks 
                      <> ["– " ++ task]
                      <> drop index1 todoTasks
      updateChangesToFile file newTodoTasks
      putStrLn $ "Task #" <> show index1 <> " updated."

remove :: Action
remove Args {..} = do
  contents <- readFile file
  let todoTasks = lines contents
      tasksCount = length todoTasks
  case validIndices index1 index2 False tasksCount of
    Left e -> putStrLn e
    Right _ -> do
      let newTodoTasks = delete (todoTasks !! (index1 - 1)) todoTasks
      updateChangesToFile file newTodoTasks
      putStrLn $ "Task #" <> show index1 <> " removed from the list."

-- Helper functions
displayTodoHeader :: IO ()
displayTodoHeader = do
  putStrLn "\t ____________ "
  putStrLn "\t|            |"
  putStrLn "\t| TODO Tasks |"
  putStrLn "\t|____________|\n"

updateChangesToFile :: FilePath -> [String] -> IO ()
updateChangesToFile file newTodoTasks = do
  (tempFile, tempHandle) <- openTempFile "." "temp"
  hPutStr tempHandle $ unlines newTodoTasks
  hClose tempHandle
  removeFile file
  renameFile tempFile file

validIndices :: Int -> Int -> Bool -> Int -> Either String Bool
validIndices i j considerJ len = 
  let withinLowerBound = not considerJ && i > 0 || i > 0 && j > 0
      withinUpperBound = not considerJ && i <= len || i <= len && j <= len
  in if withinLowerBound && withinUpperBound
      then Right True
      else if not withinLowerBound
        then Left "Invalid input for indices i and/or j."
        else Left "Indices i and/or j exceed(s) the length of the list."