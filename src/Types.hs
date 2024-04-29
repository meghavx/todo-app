module Types (
  Task, Index, Action, Command(..)
) where

type Action = Command -> IO ()

type Task = String

type Index = Int

data Command
  = Add    { file :: FilePath, task :: Task }
  | View   { file :: FilePath  }
  | Remove { file :: FilePath, i :: Index  }
  | Done   { file :: FilePath, i :: Index  }
  | Bump   { file :: FilePath, i :: Index  }
  | Move   { file :: FilePath, i :: Index, j :: Index }
  | Update { file :: FilePath, i :: Index, task :: Task }
  deriving Show