module Parsers (progParser) where

import Options.Applicative

import Types (Command(..), Index)

progParser :: ParserInfo Command
progParser = info
  (commandParser <**> helper)
  (fullDesc 
    <> header "A basic command-line TODO application"
    <> progDesc "Manage your TODO lists as text files."
    <> footer "For specific command usage, enter: --help or -h <command>")

commandParser :: Parser Command
commandParser = hsubparser $
  command "add"    (info addParser    (progDesc "Add a task")) <>
  command "view"   (info viewParser   (progDesc "View all tasks")) <>
  command "update" (info updateParser (progDesc "Update a task")) <>
  command "bump"   (info bumpParser   (progDesc "Bump a task to the top")) <>
  command "move"   (info moveParser   (progDesc "Move a task to a different position")) <>
  command "done"   (info doneParser   (progDesc "Mark a task as 'done'")) <>
  command "undone" (info undoneParser (progDesc "Unmark a task previously marked as 'done'")) <>
  command "remove" (info removeParser (progDesc "Remove a task"))

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