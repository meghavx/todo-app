{-# LANGUAGE RecordWildCards #-}

module Main where

import Types (Args(..))
import LibIO (cmdMap, fetchArgs)

main :: IO ()
main = do
  args@Args {..} <- fetchArgs
  case lookup cmd cmdMap of
    Just action -> action args
    Nothing -> putStrLn "Invalid command/args! Exiting the program."