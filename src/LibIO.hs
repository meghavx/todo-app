{-# LANGUAGE RecordWildCards #-}

module LibIO (
  fetchArgs, add, view, update, bump, move, done, undone, remove
) where

import           Data.Foldable       (for_)
import           Options.Applicative (execParser)
import           System.Process      (callCommand)
import           Data.List           (delete, dropWhileEnd)
import           System.Directory    (renameFile, removeFile)
import           System.IO           (openTempFile, hPutStr, hClose)

import           Parsers             (progParser)
import           Types               (Task, Index, Command(..), Action)
import qualified VisualEffects       as VE

fetchArgs :: IO Command
fetchArgs = execParser progParser

add :: Action
add Add {..} = do
  appendFile file $ "+" <> task <> "\n"
  VE.successFeedback "Task added to the list."

view :: Action
view View {..} = do
  (todoTasks, n) <- getTodoTasks file
  case n of
    0 -> VE.exceptionFeedback "Turns out your TODO list is empty!"
    _ -> do
      displayTodoHeader
      let numberedTasks = zip [1..n] todoTasks
      for_ numberedTasks $ \(sNo, task) -> do
        VE.putStrBold " " >> VE.putStrBoldGreen (show sNo) >> VE.putStrBold "| "
        let effect = if head task == '+' then id else VE.strikethroughEffect
        putStrLn . effect $ tail task
  VE.putStrItalicFaintLn $ "\n\n File location: " <> file <> "\n"

update :: Action
update Update {..} = do
  (todoTasks, n) <- getTodoTasks file
  if i > 0 && i <= n
    then do
      let concernedTask = todoTasks !! (i - 1)
      if head concernedTask == '+'
        then do
          editedTask <- editWithNano (tail concernedTask)
          let strippedEditedTask = dropWhileEnd (== '\n') editedTask
              newTodoTasks = updateTaskHelper todoTasks i strippedEditedTask '+'
          updateChangesToFile file newTodoTasks
          VE.successFeedback $ "Task #" <> show i <> " updated."
        else do
          VE.exceptionFeedback "Can't update a task marked as 'done'"
          VE.exceptionFeedback "Please recheck the INDEX entered OR try marking this task 'undone' first."
    else VE.errorFeedback "Invalid INDEX provided! Please view the list first."

bump :: Action
bump Bump {..} = move (Move file i 1)

move :: Action
move Move {..} = do
  (todoTasks, n) <- getTodoTasks file
  if i > 0 && i <= n && j > 0 && j <= n
    then do
      if i == j
        then VE.exceptionFeedback "No change made; both i & j point to the same position."
        else do
          let taskToMove = todoTasks !! (i - 1)
              otherTasks = delete taskToMove todoTasks
              newTodoTasks = take (j - 1) otherTasks 
                          <> [taskToMove]
                          <> drop (j - 1) otherTasks
          updateChangesToFile file newTodoTasks
          VE.successFeedback $ "Task #" <> show i <> " moved to #" <> show j <> "."
    else VE.errorFeedback "One or both INDEXES are invalid. Please view the list first."

done :: Action
done Done {..} = do 
  (todoTasks, n) <- getTodoTasks file
  if i > 0 && i <= n
    then do
      let concernedTask = todoTasks !! (i - 1)
      if head concernedTask == '+'
        then do 
          let taskToMarkAsDone = delete '+' concernedTask
              newTodoTasks = updateTaskHelper todoTasks i taskToMarkAsDone '-'
          updateChangesToFile file newTodoTasks
          VE.successFeedback $ "Task #" <> show i <> " marked as 'done'"
        else do
          VE.exceptionFeedback $ "Task #" <> show i <> " is already marked as 'done'"
    else VE.errorFeedback "Invalid INDEX provided! Please view the list first."

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
          VE.successFeedback $ "Task #" <> show i <> " restored as yet to be completed."
        else do 
          VE.exceptionFeedback $ "Task #" <> show i <> " is already in 'undone' state."
    else VE.errorFeedback "Invalid INDEX provided! Please view the list first."

remove :: Action
remove Remove {..} = do
  (todoTasks, n) <- getTodoTasks file
  if i > 0 && i <= n
    then do
      let newTodoTasks = delete (todoTasks !! (i - 1)) todoTasks
      updateChangesToFile file newTodoTasks
      VE.successFeedback $ "Task #" <> show i <> " removed from the list."
    else VE.errorFeedback "Invalid INDEX provided! Please view the list first."

-- Helper functions
displayTodoHeader :: IO ()
displayTodoHeader = do
  VE.putStrBoldLn    "\t ____________ "
  VE.putStrBoldLn    "\t|            |"
  VE.putStrBold      "\t|"
  VE.putStrBoldGreen    " TODO Tasks "
  VE.putStrBoldLn                   "|"
  VE.putStrBoldLn    "\t|____________|\n"

getTodoTasks :: FilePath -> IO ([String], Int)
getTodoTasks file = do
  contents <- readFile file
  let todoTasks = filter (/= "") $ lines contents
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

editWithNano :: String -> IO String
editWithNano initialText = do
    let tempFile = "temp.txt"
    writeFile tempFile initialText
    _ <- callCommand $ "nano " ++ tempFile
    editedText <- readFile tempFile
    removeFile tempFile
    pure editedText
