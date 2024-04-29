{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified LibIO as Lib
import           Types (Command(..))

main :: IO ()
main = do
  args <- Lib.fetchArgs
  case args of
    Add    {} -> Lib.add args
    View   {} -> Lib.view args
    Update {} -> Lib.update args
    Remove {} -> Lib.remove args
    Bump   {} -> Lib.bump args
    Move   {} -> Lib.move args
    Done   {} -> Lib.done args
    Undone {} -> Lib.undone args