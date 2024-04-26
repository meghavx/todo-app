module Parsers (progParser) where

import Types (Args(..))
import Control.Applicative ((<**>))
import qualified Options.Applicative as Options

progParser :: Options.ParserInfo Args
progParser = Options.info
  (Args
    <$> commandParser
    <*> filePathParser
    <*> todoTaskParser
    <*> todoIndexIParser
    <*> todoIndexJParser
    <**> Options.helper)
  (Options.fullDesc 
    <> Options.header "A simple TODO CLI application"
    <> Options.progDesc "Manage your TODO lists as text files.")

commandParser :: Options.Parser String
commandParser = Options.option
  Options.str (
    Options.short 'c'
    <> Options.long "cmd"
    <> Options.metavar "COMMAND"
    <> Options.help "add | view | update | remove | bump | move") 

filePathParser :: Options.Parser String
filePathParser = Options.option
  Options.str (  
    Options.short 'f'
    <> Options.long "file"
    <> Options.metavar "FILE"
    <> Options.help "Path of the file to add TODO task to")

todoTaskParser :: Options.Parser String
todoTaskParser = Options.option
  Options.str (  
    Options.short 't'
    <> Options.long "task"
    <> Options.metavar "TASK"
    <> Options.value ""
    <> Options.help "Task to add to TODO list")

todoIndexIParser :: Options.Parser Int
todoIndexIParser = Options.option
  Options.auto (  
    Options.short 'i'
    <> Options.long "index-i"
    <> Options.metavar "INDEX i"
    <> Options.value 0
    <> Options.help "Index of task to update / remove / bump / move")

todoIndexJParser :: Options.Parser Int
todoIndexJParser = Options.option
  Options.auto (  
    Options.short 'j'
    <> Options.long "index-j"
    <> Options.metavar "INDEX j"
    <> Options.value 0
    <> Options.help "Index at which to move ith task in TODO list")