module Parsers (progParser) where

import Options.Applicative

import Types (Command(..))

progParser :: ParserInfo Command
progParser = info
  (commandParser <**> helper)
  (fullDesc 
    <> header "A basic TODO application"
    <> progDesc "Manage your TODO lists as text files")

commandParser :: Parser Command
commandParser = subparser $
  command "add"    (info addParser    (progDesc "Add a task to TODO list")) <>
  command "view"   (info viewParser   (progDesc "View TODO list"))          <>
  command "update" (info updateParser (progDesc "Update ith task in TODO list"))   <>
  command "remove" (info removeParser (progDesc "Remove ith task from TODO list")) <>
  command "bump"   (info bumpParser   (progDesc "Bump ith task to the top of TODO list")) <>
  command "move"   (info moveParser   (progDesc "Move ith task in TODO list to jth position"))

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
  <*> argument auto (metavar "FROM INDEX") 
  <*> argument auto (metavar "TO INDEX `j`")
