module CLR (
  run
  ) where

-- import qualified Language.C.Parser as LC -- language-c-quote

import Types
import Parser
import CLSParser

import Prog.Args

import Control.Applicative
import Control.Monad
import Data.List
import Text.Printf
import System.IO
import System.Exit

import qualified Data.ByteString as BS

data Opts =
  Opts {
   oVerbosity :: !Int
-- , oPath :: !FilePath
  , oIterations :: !Int
  , oExprs :: ![String]
  , oProfilers :: ![Profiler]
  } deriving (Show)

dftOpts :: Opts
dftOpts =
  Opts {
    oVerbosity = 0
--  , oPath = ""
  , oIterations = 1
  , oExprs = []
  , oProfilers = []
  }

data Profiler =
    ProfilerCL
  deriving (Show,Eq)


run :: [String] -> IO ()
run as = parseArgs spec dftOpts as >>= runWithOpts


spec :: Spec Opts
spec = s
  where s = mkSpecsWithHelpOpt "clr" "" 80 [
              optF s "q" "quiet"
                "quiet output" ""
                (\o -> (o {oVerbosity = -1}))
            , optF s "v" "verbose"
                "verbose output" ""
                (\o -> (o {oVerbosity = 1}))
            , optF s "v2" "debug"
                "debug output" ""
                (\o -> (o {oVerbosity = 2}))

            , opt s "i" "iterations" "INT"
                "number of iterations to test" ""
                (\i o -> (o {oIterations = i})) # OptAttrAllowUnset

            , optG s "p" "profile options" "" [
                optF s "CL" "" "use OpenCL profiler" "Uses OpenCL event profiling"
                (\o -> (o {oProfilers = oProfilers o ++ [ProfilerCL]})) # OptAttrAllowMultiple
              ]
            ]
            [
              arg s "FILE" "expr" "a clr script" (\a o -> o{oExprs = oExprs o ++ [a]}) # OptAttrAllowMultiple
            ]


runWithOpts :: Opts -> IO ()
runWithOpts os = body
  where body = do
          mapM processExpr (oExprs os)
          return ()

        processExpr :: String -> IO ()
        processExpr inp =
          case parse pCLSSts "<expr>" inp of
            Left err -> do
              hPutStrLn stderr (fmtDiagWithLines (lines inp) err)
              exitFailure
            Right (cls,ws) -> do
              mapM_ (hPutStrLn stderr . ("WARNING: "++) . fmtDiagWithLines (lines inp)) ws
              -- compile cls
              --   * load all kernels and compile
              --   * load all buffer parameters
              --  [can I use dependent types?]
              --  buffers have implicit types (buf(foo.png)
              --  Need a "compiled IR"?
              -- could I just have compile return an (IO ())
              --  data CCLSSt =
              --    CCLSStCall
              --    CCLSStFinish
              mapM_ print cls

test = run["blend.cl`blend<1024x1024>(0:w,0:r,0:r)"]

--  inp <- readFile (oPath os)
--  length inp `seq` return ()
--  case parse (pFindKernels []) (oPath os) inp of
--    Left err -> do
--      hPutStrLn stderr (fmtDiagWithLines (lines inp) err)
--      exitFailure
--    Right (ks,ws) -> do
--      mapM_ (hPutStrLn stderr . ("WARNING: "++) . fmtDiagWithLines (lines inp)) ws
--      mapM_ (putStrLn . fmtKern) ks
--      -- print a

{-
do
  bs <- BS.readFile (oPath os)
  case LC.parse [LC.OpenCL] p bs Nothing of

  return ()


-}