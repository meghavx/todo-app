{-# LANGUAGE RecordWildCards #-}

module Main where

import Types (Command(..))
import qualified LibIO as Lib

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