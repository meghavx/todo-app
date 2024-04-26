{-# LANGUAGE RecordWildCards #-}

module LibIO (
  fetchArgs, add, view, update, remove, bump, move
) where

import System.IO
import Data.List
import System.Directory
import Options.Applicative (execParser)

import Parsers (progParser)
import Types (Command(..), Action)

fetchArgs :: IO Command
fetchArgs = execParser progParser

add :: Action
add Add {..} = do
  appendFile file $ "– " ++ task ++ "\n"
  putStrLn $ "Task added to the list."

view :: Action
view View {..} = do
  contents <- readFile file
  let todoTasks = lines contents
  case length todoTasks of
    0 -> putStrLn "Turns out your TODO list is empty!"
    _ -> do
      displayTodoHeader
      let numberedTasks = zipWith prependSNos [1 :: Int ..] todoTasks
          prependSNos n item = " " <> show n <> " " <> item
      putStrLn $ unlines numberedTasks

update :: Action
update Update {..} = do
  contents <- readFile file
  let todoTasks  = lines contents
      tasksCount = length todoTasks
  if i > 0 && i <= tasksCount
    then do
      let newTodoTasks = take (i - 1) todoTasks 
                      <> ["– " ++ task]
                      <> drop i todoTasks
      updateChangesToFile file newTodoTasks
      putStrLn $ "Task #" <> show i <> " updated."
  else putStrLn "Error: Index `i` out-of-bounds"

remove :: Action
remove Remove {..} = do
  contents <- readFile file
  let todoTasks  = lines contents
      tasksCount = length todoTasks
  if i > 0 && i <= tasksCount
    then do
      let newTodoTasks = delete (todoTasks !! (i - 1)) todoTasks
      updateChangesToFile file newTodoTasks
      putStrLn $ "Task #" <> show i <> " removed from the list."
    else putStrLn "Error: Index `i` out-of-bounds"

bump :: Action
bump Bump {..} = do
  contents <- readFile file
  let todoTasks  = lines contents
      tasksCount = length todoTasks
  if i > 0 && i <= tasksCount
    then do
      let bumpedTask = todoTasks !! (i - 1)
          newTodoTasks = bumpedTask : delete bumpedTask todoTasks
      updateChangesToFile file newTodoTasks
      putStrLn $ "Task #" <> show i <> " bumped to top of the list."
    else putStrLn "Error: Index `i` out-of-bounds"

move :: Action
move Move {..} = do
  contents <- readFile file
  let todoTasks  = lines contents
      tasksCount = length todoTasks
  if i > 0 && i <= tasksCount && j > 0 && j <= tasksCount
    then do
      if i == j
        then putStrLn "No change made; both i & j point to the same position."
        else do
          let taskToMove = todoTasks !! (i - 1)
              otherTasks = delete taskToMove todoTasks
              newTodoTasks = take (j - 1) otherTasks 
                          <> [taskToMove]
                          <> drop (j - 1) otherTasks
          updateChangesToFile file newTodoTasks
          putStrLn $ "Task #" <> show i <> " moved to #" <> show j <> "."
  else putStrLn "`i` or/and index `j` "

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