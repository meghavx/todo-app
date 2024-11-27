module Parsers (progParser) where

import Options.Applicative
import Types (Command(..), Index)

progParser :: ParserInfo Command
progParser = info
  (commandParser <**> helper)
  (fullDesc 
    <> header "A basic command-line to-do application."
    <> progDesc "Manage your todo lists as text files."
    <> footer "For specific command usage, enter --help or -h followed by the command")

commandParser :: Parser Command
commandParser = hsubparser $
  command "add" (info addParser (progDesc "Add task")) <>
  command "view" (info viewParser (progDesc "View all tasks")) <>
  command "update" (info updateParser (progDesc "Update task")) <>
  command "bump" (info bumpParser (progDesc "Bump task to the top")) <>
  command "move" (info moveParser (progDesc "Move task to a different position")) <>
  command "done" (info doneParser (progDesc "Mark task as 'done'")) <>
  command "undone" (info undoneParser (progDesc "Unmark task previously marked as 'done'")) <>
  command "remove" (info removeParser (progDesc "Remove task"))

addParser :: Parser Command
addParser = Add 
  <$> argument str (metavar "FILE") 

viewParser :: Parser Command
viewParser = View 
  <$> argument str (metavar "FILE")

updateParser :: Parser Command
updateParser = fileAndIndexParser Update

removeParser :: Parser Command
removeParser = fileAndIndexParser Remove

doneParser :: Parser Command
doneParser = fileAndIndexParser Done

undoneParser :: Parser Command
undoneParser = fileAndIndexParser Undone

bumpParser :: Parser Command
bumpParser = fileAndIndexParser Bump

moveParser :: Parser Command
moveParser = Move 
  <$> argument str  (metavar "FILE")
  <*> argument auto (metavar "FROM_INDEX") 
  <*> argument auto (metavar "TO_INDEX")

-- Helper function
fileAndIndexParser :: (FilePath -> Index -> Command) -> Parser Command
fileAndIndexParser cmd = cmd 
  <$> argument str  (metavar "FILE")
  <*> argument auto (metavar "INDEX")