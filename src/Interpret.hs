module Interpret where

import CLParser
import Fatal
import Util
import Types

import Control.Exception
import Control.Monad
import Data.List
import Data.Int
import Data.Word
import GHC.Float
import System.FilePath
import System.IO
-- import qualified Data.Map as M

import Control.Parallel.OpenCL -- opencl

data CLSInst
  = CLSIBufferAllocRW !CLMem !Int
  | CLSIBufferAllocM  !CLMem !Int
--  | CLSIBufferAllocS  !CLMem !(CLMem -> IO ())
  | CLSIBufferInit !CLMem !(CLMem -> IO ())
  | CLSICall !CLKernel
  | CLSIDiff !CLMem !CLMem
  | CLSISync


run :: String -> [CLSSt] -> IO ()
run inp css = do
  -- allocate a progam, command queue, and ...
  -- pre-allocate kernels
  -- run through and identify buffers
  -- collect constraints on each buffer (e.g. size)
  ctx <- findCtx
--  pks <- preallocateKernels ctx css []

  return ()

-- withContext :: (CLContext -> IO a) -> IO a
-- withcontext f = bracket


findCtx :: IO CLContext
findCtx = do
  ps <- clGetPlatformIDs
  dss <-
    forM ps $ \p -> do
      clGetDeviceIDs p CL_DEVICE_TYPE_GPU
  let f d = do
        str <- clGetDeviceName d
        print str
        return $ "GTX" `isInfixOf` str
  ds <- filterM f (concat dss)
  case ds of
    (d:_) -> do
      str <- clGetDeviceName d
      putStrLn $ "creating context to: " ++ str
      clCreateContext [] [d] (\s -> hPutStrLn stderr ("[ocl]: " ++ s))

-- program test = foo/bar.cl[-DT=int -DS=float]
-- test = baz.cl
data CLRInst =
    CLRInst !Pos !CLKernel !NDRange !NDRange [ArgVal]
  deriving Show
type ScalarSetter = IO ()

data ArgVal =
    ArgValScalar !(IO ())
  | ArgValBuffer !CLMem
  | ArgValImage  !CLMem
instance Show ArgVal where
  show (ArgValScalar _) = "ArgValScalar"
  show (ArgValBuffer m) = "ArgValBuffer " ++ show m
  show (ArgValImage m) = "ArgValImage " ++ show m


compileScript :: String -> CLContext -> [CLSSt] -> IO [CLRInst]
compileScript _ _   []                          = return []
compileScript inp ctx ((CLSStCall pos clsc):clsts) = body
  where body = do
          cl_inp <- readFile (clscPath clsc)
          length cl_inp `seq` return ()
          case parse (pFindKernels []) (clscPath clsc) cl_inp of
            Left err -> do
              fatal (fmtDiagWithLines (lines cl_inp) err)
            Right (ks,ws) -> do
              mapM_ (hPutStrLn stderr . ("WARNING: "++) . fmtDiagWithLines (lines cl_inp)) ws
              mapM_ (putStrLn . fmtKern) ks
              case find (\k -> kName k == clscKernel clsc) ks of
                Nothing -> fatal ("cannot find kernel " ++ clscKernel clsc)
                Just k -> do
                  p <- clCreateProgramWithSource ctx cl_inp
                  [d] <- clGetContextDevices ctx
                  let printLog s = do
                        putStrLn s
                        log <- clGetProgramBuildLog p d
                        putStrLn $ prefixLines "[log] " log
                  let handler :: CLError -> IO ()
                      handler CL_BUILD_PROGRAM_FAILURE = printLog ">>BUILD ERROR<<"
                      handler e = throwIO e
                  clBuildProgram p [d] "" `catch` handler
                  printLog ">>BUILD SUCCESS"
                  krn <- clCreateKernel p (kName k)
                  clris <- compileScript inp ctx clsts
                  args <- compileArgs pos krn (kParams k) (clscArgs clsc)
                  let clri =
                        CLRInst pos krn (clscGlobal clsc) (clscLocal clsc) args
                  return $! clri:clris

        compileArgs :: Pos -> CLKernel -> [KParam] -> [Init] -> IO [ArgVal]
        compileArgs pos clk kps inits =
          mapM (\(aix,kp,init) -> compileArg pos clk aix kp init) (zip3 [1..] kps inits)

        compileArg :: Pos -> CLKernel -> Int -> KParam -> Init -> IO ArgVal
        compileArg pos clk aix kp = compileInit
          where argError :: String -> IO a
                argError msg =
                  fatal $ fmtDiagWithLines (lines inp) (Diag pos ("ERROR: arg " ++ show aix ++ ": " ++ msg))

                compileInit :: Init -> IO ArgVal
                compileInit (InitInt _ i64)
                  | kpQual kp == KQualPrivate = do
                  let setScalarArg a = return . ArgValScalar $ do
                        clSetKernelArgSto clk (fromIntegral aix) a
                  case kpType kp of
                    PrimType TGroupSInt 1 1 ->
                      setScalarArg (fromIntegral i64 :: Int8)
                    PrimType TGroupSInt 1 2 ->
                      setScalarArg (fromIntegral i64 :: Int16)
                    PrimType TGroupSInt 1 4 ->
                      setScalarArg (fromIntegral i64 :: Int32)
                    PrimType TGroupSInt 1 8 ->
                      setScalarArg (fromIntegral i64 :: Int64)

                    PrimType TGroupUInt 1 1 ->
                      setScalarArg (fromIntegral i64 :: Word8)
                    PrimType TGroupUInt 1 2 ->
                      setScalarArg (fromIntegral i64 :: Word16)
                    PrimType TGroupUInt 1 4 ->
                      setScalarArg (fromIntegral i64 :: Word32)
                    PrimType TGroupUInt 1 8 ->
                      setScalarArg (fromIntegral i64 :: Word64)

                    PrimType TGroupFlt 1 2 -> -- TODO: need flt16 support (use half library?)
                      setScalarArg (fromIntegral i64 :: Word16)
                    PrimType TGroupFlt 1 4 ->
                      setScalarArg (fromIntegral i64 :: Float)
                    PrimType TGroupFlt 1 8 ->
                      setScalarArg (fromIntegral i64 :: Double)
                    _ -> argError $ "kernel formal parameter: unsupported type for integral arg " ++ show (kpType kp)

-- TODO: rename PrimType to FormalVal and use ActVal
-- TODO: vector init should be defined as broadcast
--       (i.e. change the Init to explicitly be 1 -> {1,1,1,1})
                compileInit (InitFlt _ f64)
                  | kpQual kp == KQualPrivate = do
                  case kpType kp of
                    -- PrimType TGroupFlt 1 2 -> -- TODO: need flt16 support (use half library?)
                    --  clSetKernelArgSto clk (fromIntegral aix) (fromIntegral i64 :: Word16)
                    PrimType TGroupFlt 1 4 ->
                      setScalarArg (double2Float f64 :: Float)
                    PrimType TGroupFlt 1 8 ->
                      setScalarArg f64
                    _ -> argError $ "kernel formal parameter: unsupported type for floating arg " ++ show (kpType kp)

                  | kpQual kp == KQualGlobal || kpQual kp == KQualConstant =
                  case kpType kp of
                    PrimType TGroupSInt 1 1 -> do -- allocate buffer with constant values
                      error "allocate a buffer"
                    _ -> argError $ "kernel formal parameter: unsupported type for buffer arg " ++ show (kpType kp)
                  where setScalarArg a = return . ArgValScalar $ do
                          clSetKernelArgSto clk (fromIntegral aix) a

{-
preallocateKernels ::
  CLContext ->
  [CLSSt] ->
  [(CLSCall,Kern,CLProgram,CLKernel)] ->
  IO [(Pos,CLKernel)]
preallocateKernels ctx ((CLSStCall pos clsc):clsts) pks = do
  inp <- readFile (clscPath clsc)
  -- length inp `length` return ()
  case parse (pFindKernels []) (clscPath clsc) inp of
    Left err -> do
      fatal (fmtDiagWithLines (lines inp) err)

    Right (ks,ws) -> do
      mapM_ (hPutStrLn stderr . ("WARNING: "++) . fmtDiagWithLines (lines inp)) ws
      mapM_ (putStrLn . fmtKern) ks
      case find (\k -> kName k == clscKernel clsc) ks of
        Nothing -> fatal ("cannot find kernel " ++ clscKernel clsc)
        Just k -> do
          p <- clCreateProgramWithSource ctx inp
          print "clGetContextDevices"
          [d] <- clGetContextDevices ctx
          let printLog s = do
                putStrLn s
                log <- clGetProgramBuildLog p d
                putStrLn $ prefixLines "[log] " log
          let handler :: CLError -> IO ()
              handler CL_BUILD_PROGRAM_FAILURE = printLog ">>BUILD ERROR<<"
              handler e = throwIO e
          clBuildProgram p [d] "" `catch` handler
          printLog ">>BUILD SUCCESS"
          krn <- clCreateKernel p (kName k)
          preallocateKernels ctx clsts (pos,krn,p,k)
-}