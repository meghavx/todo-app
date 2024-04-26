module Types (
  Action,
  Args(..)
) where

-- Type to store arguments provided by the user
data Args = Args 
  { cmd    :: String
  , file   :: FilePath
  , task   :: String
  , index1 :: Int
  , index2 :: Int
} deriving Show

type Action = Args -> IO ()
