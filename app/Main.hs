module Main where

import LibIO (fetchArgs, handle)

main :: IO ()
main = do
  args <- fetchArgs
  handle args