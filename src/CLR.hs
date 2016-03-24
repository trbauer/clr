module CLR (
  runWithOpts
  ) where

-- import qualified Language.C.Parser as LC -- language-c-quote

import Types
import Parser

import Prog.Args

import Control.Applicative
import Control.Monad
import Data.List
import Text.Printf
import System.IO
import System.Exit

import qualified Data.ByteString as BS

data Opts = Opts {
   oVerbosity :: !Int
 , oPath :: !FilePath
 } deriving (Show)


dftOpts :: Opts
dftOpts = Opts {
    oVerbosity = 0
  , oPath = ""
  }

run :: [String] -> IO ()
run as = parseArgs spec dftOpts as >>= runWithOpts


spec :: Spec Opts
spec = s
  where s = mkSpecsWithHelpOpt "clr" "" 80 [
              flag s "q" "quiet"
                "quiet output" ""
                (\o -> (o {oVerbosity = -1}))
            , flag s "v" "verbose"
                "verbose output" ""
                (\o -> (o {oVerbosity = 1}))
            , flag s "v2" "debug"
                "debug output" ""
                (\o -> (o {oVerbosity = 2}))
            ]
            [
              arg s "ARG1" "the arg" "long arg desc" (\a o -> o{oPath = a})
            ]


runWithOpts :: Opts -> IO ()
runWithOpts os = do
  inp <- readFile (oPath os)
  length inp `seq` return ()
  case parse (pFindKernels []) (oPath os) inp of
    Left err -> do
      hPutStrLn stderr (fmtDiagWithLines (lines inp) err)
      exitFailure
    Right (ks,ws) -> do
      mapM_ (hPutStrLn stderr . ("WARNING: "++) . fmtDiagWithLines (lines inp)) ws
      mapM_ (putStrLn . fmtKern) ks
      -- print a

{-
do
  bs <- BS.readFile (oPath os)
  case LC.parse [LC.OpenCL] p bs Nothing of

  return ()


-}