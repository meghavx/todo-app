module Types where

type Index = Int

data Command
  = Add    FilePath
  | View   FilePath 
  | Update FilePath Index 
  | Remove FilePath Index  
  | Done   FilePath Index  
  | Undone FilePath Index  
  | Bump   FilePath Index  
  | Move   FilePath Index Index
  deriving Show