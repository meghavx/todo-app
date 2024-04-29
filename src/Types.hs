module Types where

type Index = Int

type Task = String

data Command
  = Add    { file :: FilePath, task :: Task }
  | View   { file :: FilePath  }
  | Update { file :: FilePath, i :: Index  }
  | Remove { file :: FilePath, i :: Index  }
  | Done   { file :: FilePath, i :: Index  }
  | Undone { file :: FilePath, i :: Index  }
  | Bump   { file :: FilePath, i :: Index  }
  | Move   { file :: FilePath, i :: Index, j :: Index }
  deriving Show

type Action = Command -> IO ()