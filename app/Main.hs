{-# LANGUAGE RecordWildCards #-}

module Main where

import Types (Command(..))
import LibIO (fetchArgs, add, view, update, remove, bump, move)

main :: IO ()
main = do
  args <- fetchArgs
  case args of
    Add    {} -> add args
    View   {} -> view args
    Update {} -> update args
    Remove {} -> remove args
    Bump   {} -> bump args
    Move   {} -> move args
