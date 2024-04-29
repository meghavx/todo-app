{-# LANGUAGE RecordWildCards #-}

module LibIO (
  fetchArgs, add, view, update, remove, bump, move, done
) where

import System.IO
import System.Directory
import Data.List (delete)
import Options.Applicative (execParser)

import Parsers (progParser)
import Types (Command(..), Action, Task, Index)

fetchArgs :: IO Command
fetchArgs = execParser progParser

add :: Action
add Add {..} = do
  appendFile file $ "+ " <> task <> "\n"
  putStrLn $ "Task added to the list."

view :: Action
view View {..} = do
  (todoTasks, n) <- getTodoTasks file
  case n of
    0 -> putStrLn "Turns out your TODO list is empty!"
    _ -> do
      displayTodoHeader
      let numberedTasks = zipWith prependSerialNos [1..n] todoTasks
          prependSerialNos n item = " " <> show n <> "| " <> item
      putStrLn $ unlines numberedTasks

update :: Action
update Update {..} = do
  (todoTasks, n) <- getTodoTasks file
  if i > 0 && i <= n
    then do
      let newTodoTasks = updateTaskHelper todoTasks i task "+ "
      updateChangesToFile file newTodoTasks
      putStrLn $ "Task #" <> show i <> " updated."
    else putStrLn "Error: Invalid INDEX. You might want to view the list first."

remove :: Action
remove Remove {..} = do
  (todoTasks, n) <- getTodoTasks file
  if i > 0 && i <= n
    then do
      let newTodoTasks = delete (todoTasks !! (i - 1)) todoTasks
      updateChangesToFile file newTodoTasks
      putStrLn $ "Task #" <> show i <> " removed from the list."
    else putStrLn "Error: Invalid INDEX. You might want to view the list first."

bump :: Action
bump Bump {..} = move (Move file i 1)

move :: Action
move Move {..} = do
  (todoTasks, n) <- getTodoTasks file
  if i > 0 && i <= n && j > 0 && j <= n
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
    else putStrLn "Error: One or both INDEXES are invalid. You might want to view the list first."

done :: Action
done Done {..} = do 
  (todoTasks, n) <- getTodoTasks file
  if i > 0 && i <= n
    then do
      let taskToMarkAsDone = delete '+' $ todoTasks !! (i - 1)
          newTodoTasks = updateTaskHelper todoTasks i taskToMarkAsDone "-"
      updateChangesToFile file newTodoTasks 
      putStrLn $ "Task #" <> show i <> " marked as 'done'."
    else putStrLn "Error: Invalid INDEX. You might want to view the list first."


-- Helper functions
displayTodoHeader :: IO ()
displayTodoHeader = do
  putStrLn "\t ____________ "
  putStrLn "\t|            |"
  putStrLn "\t| TODO Tasks |"
  putStrLn "\t|____________|\n"

getTodoTasks :: FilePath -> IO ([String], Int)
getTodoTasks file = do
  contents <- readFile file
  let todoTasks = lines contents
  pure (todoTasks, length todoTasks)

updateTaskHelper :: [Task] -> Index -> Task -> String -> [Task]
updateTaskHelper todoTasks i task prefix = 
  take (i - 1) todoTasks 
  <> [prefix <> task]
  <> drop i todoTasks

updateChangesToFile :: FilePath -> [String] -> IO ()
updateChangesToFile file newTodoTasks = do
  (tempFile, tempHandle) <- openTempFile "." "temp"
  hPutStr tempHandle $ unlines newTodoTasks
  hClose tempHandle
  removeFile file
  renameFile tempFile file
