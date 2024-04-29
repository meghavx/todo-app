{-# LANGUAGE RecordWildCards #-}

module LibIO (
  fetchArgs, add, view, update, remove, bump, move, done, undone
) where

import Data.Foldable       (for_)
import Data.List           (delete)
import Options.Applicative (execParser)
import System.Directory    (renameFile, removeFile)
import System.IO           (openTempFile, hPutStr, hClose)
import String.ANSI         (bold, italic, faint, strikethrough, green)

import Parsers             (progParser)
import Types               (Task, Index, Command(..), Action)

fetchArgs :: IO Command
fetchArgs = execParser progParser

add :: Action
add Add {..} = do
  appendFile file $ "+" <> task <> "\n"
  putStrLn $ "Task added to the list."

view :: Action
view View {..} = do
  (todoTasks, n) <- getTodoTasks file
  case n of
    0 -> putStrLn "Turns out your TODO list is empty!"
    _ -> do
      displayTodoHeader
      let numberedTasks = zip [1..n] todoTasks
      for_ numberedTasks $ \(sNo, task) -> do
        putStrB $ " " <> (green . show) sNo <> "| "
        let effect = if head task == '+' then id else strikethrough . italic . faint
        putStrLn . effect $ tail task
  putStrLn ""

update :: Action
update Update {..} = do
  (todoTasks, n) <- getTodoTasks file
  if i > 0 && i <= n
    then do
      let concernedTask = todoTasks !! (i - 1)
      if head concernedTask == '+'
        then do 
          let newTodoTasks = updateTaskHelper todoTasks i task '+'
          updateChangesToFile file newTodoTasks
          putStrLn $ "Task #" <> show i <> " updated."
        else putStrLn $ "Updation failed since task #" <> show i <> " is marked as done / completed."
    else displayInvalidIndexMsg

remove :: Action
remove Remove {..} = do
  (todoTasks, n) <- getTodoTasks file
  if i > 0 && i <= n
    then do
      let newTodoTasks = delete (todoTasks !! (i - 1)) todoTasks
      updateChangesToFile file newTodoTasks
      putStrLn $ "Task #" <> show i <> " removed from the list."
    else displayInvalidIndexMsg

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
          newTodoTasks = updateTaskHelper todoTasks i taskToMarkAsDone '-'
      updateChangesToFile file newTodoTasks 
      putStrLn $ "Task #" <> show i <> " marked as 'done'."
    else displayInvalidIndexMsg

undone :: Action
undone Undone {..} = do
  (todoTasks, n) <- getTodoTasks file
  if i > 0 && i <= n
    then do
      let concernedTask = todoTasks !! (i - 1)
      if head concernedTask == '-'
        then do 
          let modifiedTask = delete '-' concernedTask
              newTodoTasks = updateTaskHelper todoTasks i modifiedTask '+'
          updateChangesToFile file newTodoTasks
          putStrLn $ 
            "Task #" <> show i <> " restored as undone / yet to be completed."
        else putStrLn $ "Task #" <> show i <> " is yet to be marked as 'done'; hence is already in the 'undone' state."
    else displayInvalidIndexMsg

-- Helper functions
displayTodoHeader :: IO ()
displayTodoHeader = do
  putStrBLn       "\t ____________ "
  putStrBLn       "\t|            |"
  putStrB         "\t|"
  putStrB . green $  " TODO Tasks "
  putStrBLn                      "|"
  putStrBLn       "\t|____________|\n"

putStrB :: String -> IO ()
putStrB = putStr . bold

putStrBLn :: String -> IO ()
putStrBLn = putStrLn . bold

getTodoTasks :: FilePath -> IO ([String], Int)
getTodoTasks file = do
  contents <- readFile file
  let todoTasks = lines contents
  pure (todoTasks, length todoTasks)

updateTaskHelper :: [Task] -> Index -> Task -> Char -> [Task]
updateTaskHelper todoTasks i task prefix = 
  take (i - 1) todoTasks 
  <> [prefix : task]
  <> drop i todoTasks

updateChangesToFile :: FilePath -> [String] -> IO ()
updateChangesToFile file newTodoTasks = do
  (tempFile, tempHandle) <- openTempFile "." "temp"
  hPutStr tempHandle $ unlines newTodoTasks
  hClose tempHandle
  removeFile file
  renameFile tempFile file

displayInvalidIndexMsg :: IO ()
displayInvalidIndexMsg = 
  putStrLn "Error: Invalid INDEX. You might want to view the list first."