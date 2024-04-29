module Parsers (progParser) where

import Options.Applicative

import Types (Command(..))

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
  command "remove" (info removeParser (progDesc "Remove a task")) <>
  command "done"   (info doneParser   (progDesc "Mark a task as 'done'"))

addParser :: Parser Command
addParser = Add 
  <$> argument str (metavar "FILE") 
  <*> argument str (metavar "TASK")

viewParser :: Parser Command
viewParser = View 
  <$> argument str (metavar "FILE")

updateParser :: Parser Command
updateParser = Update 
  <$> argument str  (metavar "FILE") 
  <*> argument auto (metavar "INDEX") 
  <*> argument str  (metavar "TASK")

bumpParser :: Parser Command
bumpParser = Bump 
  <$> argument str  (metavar "FILE")
  <*> argument auto (metavar "INDEX")

moveParser :: Parser Command
moveParser = Move 
  <$> argument str  (metavar "FILE")
  <*> argument auto (metavar "FROM_INDEX") 
  <*> argument auto (metavar "TO_INDEX")

removeParser :: Parser Command
removeParser = Remove 
  <$> argument str  (metavar "FILE") 
  <*> argument auto (metavar "INDEX")

doneParser :: Parser Command
doneParser = Done 
  <$> argument str  (metavar "FILE")
  <*> argument auto (metavar "INDEX")
