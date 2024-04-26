module Types (Command(..), Action) where

type Action = Command -> IO ()

data Command
  = Add    { file :: FilePath, task :: String }
  | View   { file :: FilePath  }
  | Remove { file :: FilePath, i :: Int  }
  | Bump   { file :: FilePath, i :: Int  }
  | Move   { file :: FilePath, i :: Int, j :: Int }
  | Update { file :: FilePath, i :: Int, task :: String }
  deriving Show