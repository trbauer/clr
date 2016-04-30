module Fatal where

import Control.Exception
import System.IO
import System.Exit


fatal :: String -> IO a
fatal msg = do
  hPutStrLn stderr msg
  exitFailure

infixr 0 <!>
(<!>) :: IO a -> String -> IO a
(<!>) op api = op `catch` handler
  where handler :: SomeException -> IO a
        handler e = do
          hPutStrLn stderr $ "=> " ++ api
          throwIO e