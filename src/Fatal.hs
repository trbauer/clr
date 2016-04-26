module Fatal where

import System.IO
import System.Exit


fatal :: String -> IO a
fatal msg = do
  hPutStrLn stderr msg
  exitFailure