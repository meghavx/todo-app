module LibIO (fetchArgs, handle) where

import qualified Data.Text           as T
import qualified Data.Text.IO        as TIO
import           Data.Foldable       (for_)
import           Control.Monad       (when)
import           Options.Applicative (execParser)
import           System.Process      (callCommand)
import           Data.List           (delete)
import           System.IO           (openTempFile, hClose)
import           System.Directory    (renameFile, removeFile)     

import qualified VisualEffects       as VE
import           Parsers             (progParser)
import           Types               (Command(..), Index)

fetchArgs :: IO Command
fetchArgs = execParser progParser

handle :: Command -> IO ()
handle cmd = case cmd of
  Add    file     -> add    file 
  View   file     -> view   file
  Update file i   -> update file i
  Remove file i   -> remove file i
  Bump   file i   -> bump   file i
  Move   file i j -> move   file i j
  Done   file i   -> done   file i
  Undone file i   -> undone file i

add :: FilePath -> IO ()
add file = do
  task <- editWithNano $ T.pack ""
  when (not $ T.null task) $ do
    TIO.appendFile file $ T.pack "+" <> task <> T.pack "\n"
    VE.successFeedback . T.pack $ "Task added to the list."

view :: FilePath -> IO ()
view file = do
  (todoTasks, n) <- getTodoTasks file
  case n of
    0 -> VE.exceptionFeedback $ T.pack "Turns out your TODO list is empty!"
    _ -> do
      displayTodoHeader
      let numberedTasks = zip [1..n] todoTasks
      for_ numberedTasks $ \(sNo, task) -> do
        VE.putStrBold (T.pack " ") >> VE.putStrBoldGreen (T.pack $ show sNo) >> VE.putStrBold (T.pack " ")
        let effect = if T.head task == '+' then id else VE.strikethroughEffect
        TIO.putStrLn . effect $ T.tail task
  VE.putStrItalicFaintGreenLn . T.pack $ "\n File location: " <> file <> "\n"

update :: FilePath -> Index -> IO ()
update file i = do
  (todoTasks, n) <- getTodoTasks file
  if i > 0 && i <= n
    then do
      let concernedTask = todoTasks !! (i - 1)
      if T.head concernedTask == '+'
        then do
          editedTask <- editWithNano (T.tail concernedTask)
          let newTodoTasks = updateTaskHelper todoTasks i editedTask '+'
          updateChangesToFile file newTodoTasks
          VE.successFeedback . T.pack $ "Task #" <> show i <> " updated."
        else do
          VE.exceptionFeedback . T.pack $ "Can't update a task marked as 'done'"
          VE.exceptionFeedback . T.pack $ "Please recheck the INDEX entered OR try marking this task 'undone' first."
    else displayInvalidIndexErrorMsg

bump :: FilePath -> Index -> IO ()
bump file i = move file i 1

move :: FilePath -> Index -> Index -> IO ()
move file i j = do
  (todoTasks, n) <- getTodoTasks file
  if i > 0 && i <= n && j > 0 && j <= n
    then do
      when (i /= j) $ do
        let taskToMove = todoTasks !! (i - 1)
            otherTasks = delete taskToMove todoTasks
            newTodoTasks = 
              take (j - 1) otherTasks 
              <> [taskToMove]
              <> drop (j - 1) otherTasks
        updateChangesToFile file newTodoTasks
        VE.successFeedback . T.pack $ "Task #" <> show i <> " moved to #" <> show j <> "."
    else displayInvalidIndexErrorMsg

done :: FilePath -> Index -> IO ()
done file i = do 
  (todoTasks, n) <- getTodoTasks file
  if i > 0 && i <= n
    then do
      let concernedTask = todoTasks !! (i - 1)
      when (T.head concernedTask == '+') $ do
        let taskToMarkAsDone = T.tail concernedTask
            newTodoTasks = updateTaskHelper todoTasks i taskToMarkAsDone '-'
        updateChangesToFile file newTodoTasks
        VE.successFeedback . T.pack $ "Task #" <> show i <> " marked as 'done'"
    else displayInvalidIndexErrorMsg

undone :: FilePath -> Index -> IO ()
undone file i = do
  (todoTasks, n) <- getTodoTasks file
  if i > 0 && i <= n
    then do
      let concernedTask = todoTasks !! (i - 1)
      when (T.head concernedTask == '-') $ do
        let modifiedTask = T.tail concernedTask
            newTodoTasks = updateTaskHelper todoTasks i modifiedTask '+'
        updateChangesToFile file newTodoTasks
        VE.successFeedback . T.pack $ "Task #" <> show i <> " restored as yet to be completed."
    else displayInvalidIndexErrorMsg

remove :: FilePath -> Index -> IO ()
remove file i = do
  (todoTasks, n) <- getTodoTasks file
  if i > 0 && i <= n
    then do
      let newTodoTasks = [task | (pos, task) <- zip [1..n] todoTasks, pos /= i] 
      updateChangesToFile file newTodoTasks
      VE.successFeedback . T.pack $ "Task #" <> show i <> " removed from the list."
    else displayInvalidIndexErrorMsg

-- Helper functions
displayTodoHeader :: IO ()
displayTodoHeader = do
  VE.putStrBoldLn    $ T.pack "\t ____________ "
  VE.putStrBoldLn    $ T.pack "\t|            |"
  VE.putStrBold      $ T.pack "\t|"
  VE.putStrBoldGreen $ T.pack    " TODO Tasks "
  VE.putStrBoldLn    $ T.pack                "|"
  VE.putStrBoldLn    $ T.pack "\t|____________|\n"

getTodoTasks :: FilePath -> IO ([T.Text], Int)
getTodoTasks file = do
  contents <- TIO.readFile file
  let todoTasks = T.lines contents
  pure (todoTasks, length todoTasks)

updateTaskHelper :: [T.Text] -> Index -> T.Text -> Char -> [T.Text]
updateTaskHelper todoTasks i task prefix = 
  take (i - 1) todoTasks 
  <> [T.pack $ prefix : T.unpack task]
  <> drop i todoTasks

updateChangesToFile :: FilePath -> [T.Text] -> IO ()
updateChangesToFile file newTodoTasks = do
  (tempFile, tempHandle) <- openTempFile "." "temp"
  TIO.hPutStr tempHandle $ T.unlines newTodoTasks
  hClose tempHandle
  removeFile file
  renameFile tempFile file

editWithNano :: T.Text -> IO T.Text
editWithNano initialText = do
    let tempFile = "temp.txt"
    TIO.writeFile tempFile $ initialText
    _ <- callCommand $ "nano " ++ tempFile
    editedText <- TIO.readFile tempFile
    removeFile tempFile
    let editedText' = T.dropWhileEnd (== '\n') editedText
    pure editedText'

displayInvalidIndexErrorMsg :: IO ()
displayInvalidIndexErrorMsg = VE.errorFeedback . T.pack $ 
  "Invalid INDEX value(s) provided! Please view the list first."
