module VisualEffects where

import Text.ANSI
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

putStrBold :: T.Text -> IO ()
putStrBold = TIO.putStr . bold

putStrBoldLn :: T.Text -> IO ()
putStrBoldLn = TIO.putStrLn . bold

putStrItalicFaintGreenLn :: T.Text -> IO ()
putStrItalicFaintGreenLn = TIO.putStrLn . italic . faint . green

putStrBoldGreen :: T.Text -> IO () 
putStrBoldGreen = putStrBold . green

successFeedback :: T.Text -> IO ()
successFeedback = TIO.putStrLn . green

exceptionFeedback :: T.Text -> IO ()
exceptionFeedback = TIO.putStrLn . magenta

errorFeedback :: T.Text -> IO ()
errorFeedback = TIO.putStrLn . red

strikethroughEffect :: T.Text -> T.Text
strikethroughEffect = strikethrough . italic . faint
