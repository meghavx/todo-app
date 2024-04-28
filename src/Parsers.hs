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
  command "add"    (info addParser    (progDesc "Add task to TODO file")) <>
  command "view"   (info viewParser   (progDesc "View tasks in TODO file")) <>
  command "update" (info updateParser (progDesc "Update task in TODO file")) <>
  command "remove" (info removeParser (progDesc "Remove task from TODO file")) <>
  command "bump"   (info bumpParser   (progDesc "Bump task to the top in TODO file")) <>
  command "move"   (info moveParser   (progDesc "Move task to a different position in TODO file"))

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

removeParser :: Parser Command
removeParser = Remove 
  <$> argument str  (metavar "FILE") 
  <*> argument auto (metavar "INDEX")

bumpParser :: Parser Command
bumpParser = Bump 
  <$> argument str  (metavar "FILE")
  <*> argument auto (metavar "INDEX")

moveParser :: Parser Command
moveParser = Move 
  <$> argument str  (metavar "FILE")
  <*> argument auto (metavar "FROM_INDEX") 
  <*> argument auto (metavar "TO_INDEX")
