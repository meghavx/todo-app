module VisualEffects where

import String.ANSI

putStrBold :: String -> IO ()
putStrBold = putStr . bold

putStrBoldLn :: String -> IO ()
putStrBoldLn = putStrLn . bold

putStrItalicFaintGreenLn :: String -> IO ()
putStrItalicFaintGreenLn = putStrLn . italic . faint . green

putStrBoldGreen :: String -> IO () 
putStrBoldGreen = putStrBold . green

successFeedback :: String -> IO ()
successFeedback = putStrLn . green

exceptionFeedback :: String -> IO ()
exceptionFeedback = putStrLn . magenta

errorFeedback :: String -> IO ()
errorFeedback = putStrLn . red

strikethroughEffect :: String -> String
strikethroughEffect = strikethrough . italic . faint
