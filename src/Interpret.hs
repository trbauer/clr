module Interpret where

import CLParser
import Fatal
import Util
import Types

import Control.Exception
import Control.Monad
import Data.Int
import Data.List
import Data.Word
import Foreign.Ptr
import Foreign.Storable
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


run :: Int -> String -> [CLSSt] -> IO ()
run v inp css = do
  -- allocate a progam, command queue, and ...
  -- pre-allocate kernels
  -- run through and identify buffers
  -- collect constraints on each buffer (e.g. size)
  ctx <- findCtx
--  pks <- preallocateKernels ctx css []
  compileScript v inp ctx css emptyScript

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
-- data CLRInst =
--     CLRInst !Pos !CLKernel !NDRange !NDRange [ArgVal]
--   deriving Show
-- type ScalarSetter = IO ()
--
-- data ArgVal =
--     ArgValScalar !(IO ())
--   | ArgValBuffer !CLMem
--   | ArgValImage  !CLMem
-- instance Show ArgVal where
--   show (ArgValScalar _) = "ArgValScalar"
--  show (ArgValBuffer m) = "ArgValBuffer " ++ show m
--  show (ArgValImage m) = "ArgValImage " ++ show m
--

data Script =
  Script {
    -- called once before each sampling iteration
    scItrInit :: !(CLCommandQueue -> IO ())
    -- call
  , scItrRun :: !(CLCommandQueue -> IO ())
  }
emptyScript :: Script
emptyScript =
  Script {
    scItrInit = \_ -> return ()
  , scItrRun = \_ -> return ()
  }

compileScript :: Int -> String -> CLContext -> [CLSSt] -> Script -> IO Script
compileScript _ _ _   []                             scr = return scr
compileScript v inp ctx ((CLSStCall pos clsc):clsts) scr0 = body
  where body = do
          putStrLn $ show clsc
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
                  debugLn $ "creating kernel\n" ++ fmtKern k
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
                  scr1 <- compileArgs clsc pos krn (kParams k) (clscArgs clsc) scr0
                  compileScript v inp ctx clsts scr1
                  -- let clri =
                  --      CLRInst pos krn (clscGlobal clsc) (clscLocal clsc) args
                  -- return $! clri:clris

        debugLn :: String -> IO ()
        debugLn msg
          | v > 1 = putStrLn msg
          | otherwise = return ()
        verboseLn :: String -> IO ()
        verboseLn msg
          | v > 0 = putStrLn msg
          | otherwise = return ()



        compileArgs :: CLSCall -> Pos -> CLKernel -> [KParam] -> [Init] -> Script -> IO Script
        compileArgs clsc pos clk kps inits scr0 = go 1 kps inits scr0
          where go aix (kp:kps) (init:inits) scr0 = compileArg clsc pos clk aix kp init scr0 >>= go (aix + 1) kps inits
                go _   []       []           scr  = return scr


        compileArg :: CLSCall -> Pos -> CLKernel -> Int -> KParam -> Init -> Script -> IO Script
        compileArg clsc pos clk aix kp init scr0 = body
          where argError :: String -> IO a
                argError = argErrorAt pos
                argErrorAt :: Pos -> String -> IO a
                argErrorAt pos msg =
                  fatal $ fmtDiagWithLines (lines inp) (Diag pos ("ERROR: arg " ++ show aix ++ ": " ++ msg))

                body = do
                  debugLn $ "compiling arg " ++ show aix ++ "\n" ++
                            show init
                  compileInit init scr0

                compileInit :: Init -> Script -> IO Script
                compileInit (InitInt _ i64) scr0
                  | kpQual kp /= KQualPrivate = argError "scalar inits must target private args (uniform)"
                  | otherwise = do
                  let setScalarArg a = do
                        -- supports any storable value a
                        debugLn $ "clSetKernelArgSto(" ++
                          show clk ++ ", " ++
                          show aix ++ ", " ++
                          show a ++ ")"
                        print $ sizeOf a
                        clSetKernelArgSto clk (fromIntegral aix) a <!> "clSetKernelArgSto"

                        return $ scr0 {
                          scItrInit = \cq -> do
                            scItrInit scr0 cq
                            debugLn $ "clSetKernelArgSto(" ++
                              show clk ++ ", " ++
                              show aix ++ ", " ++
                              show a ++ ")"
                            clSetKernelArgSto clk (fromIntegral aix) a <!> "clSetKernelArgSto"
                        }
                  case kpType kp of
                    PrimType TGroupSInt 1 1 ->
                      setScalarArg (fromIntegral i64 :: Int8)
                    PrimType TGroupSInt 2 1 -> do
                      let b = fromIntegral i64 :: Int8
                      setScalarArg $ Vec2 b b

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

--       (i.e. change the Init to explicitly be 1 -> {1,1,1,1})
                compileInit (InitFlt _ f64) scr0
                  | kpQual kp /= KQualPrivate = argError "scalar inits must target private args (uniform)"
                  | otherwise = do
                  case kpType kp of
                    -- PrimType TGroupFlt 1 2 -> -- TODO: need flt16 support (use half library?)
                    --  clSetKernelArgSto clk (fromIntegral aix) (fromIntegral i64 :: Word16)
                    PrimType TGroupFlt 1 4 ->
                      setScalarArg (double2Float f64 :: Float)
                    PrimType TGroupFlt 1 8 ->
                      setScalarArg f64
                    _ -> argError $ "kernel formal parameter: unsupported type for floating arg " ++ show (kpType kp)

                  where setScalarArg a = do
                          debugLn $ "clSetKernelArgSto(" ++
                            show clk ++ ", " ++
                            show aix ++ ", " ++
                            show a ++ ")"
                          return $ scr0 {
                            scItrInit = \cq -> do
                              scItrInit scr0 cq
                              clSetKernelArgSto clk (fromIntegral aix) a <!> "clSetKernelArgSto"
                          }
                compileInit (InitBuf _ init_val msz bacc btrans) scr0
                  -- if this fails, throw a fit
                  | kpQual kp /= KQualGlobal && kpQual kp /= KQualConstant = argError "buffer inits require global or constant address space"
                  | otherwise = do
                  case kpType kp of
                    -- TODO: generalize
                    PointerType (PrimType ty elems_per_item bytes_per_elem) -> do
                      nelems <-
                        case msz of
                          Just sx -> evalSizeExpr sx
                          Nothing -> inferBufferSizeFromNDR (clscPos clsc) (clscGlobal clsc)
                            -- 1 element per work item
                      -- autosize buffer
                      -- => so the initializer can write it
                      -- initToStamper init_val
                      -- let write ptr ix = pokeElemOff ptr ix (fromIntegral i64 :: Int8)

                      let bytes_per_item = elems_per_item * bytes_per_elem
                          buf_bytes = nelems * bytes_per_elem
                          buf_bytes_str
                            | buf_bytes `mod` (1024*1024) == 0 = show (buf_bytes`div`(1024*1024)) ++ "M"
                            | buf_bytes `mod` 1024 == 0 = show (buf_bytes`div`1024) ++ "K"
                            | otherwise = show buf_bytes ++ "B"
                          mflags = [buf_acc,CL_MEM_ALLOC_HOST_PTR]
                            where buf_acc =
                                    case bacc of
                                      BufAccR -> CL_MEM_READ_ONLY
                                      BufAccW -> CL_MEM_WRITE_ONLY
                                      BufAccRW -> CL_MEM_READ_WRITE
                      debugLn $ "clCreateBuffer ("++show ctx++", "++
                                  intercalate "|" (map show mflags)++", " ++
                                  buf_bytes_str ++ ", nullptr)"

                      -- create a backing buffer
                      buf <- clCreateBuffer ctx mflags (buf_bytes,nullPtr) <!> "clCreateBuffer"
                      debugLn $ show buf ++ " <="
                      debugLn $ "clSetKernelArg ("++show clk++", "++show aix++", " ++ show (sizeOf buf) ++ ", " ++ show buf ++ ")"
                      clSetKernelArg clk (fromIntegral aix) (sizeOf buf) buf <!> "clSetKernelArg"

                      return $!
                        scr0 {
                          scItrInit = \cq -> do
                            scItrInit scr0 cq
                            error "IMPLEMENT ME: allocate buffer"
                        }
                    _ -> argError $ "kernel formal parameter: unsupported type for buffer arg " ++ show (kpType kp)

                inferBufferSizeFromNDR :: Pos -> NDRange -> IO Int
                inferBufferSizeFromNDR p ndr =
                  case ndr of
                    NDRNull -> argErrorAt p "cannot evaluate buffer size without NDRange size"
                    NDR1D x -> return x
                    NDR2D x y -> return $! x*y
                    NDR3D x y z -> return $! x*y*z

                -- compileArg.evalSizeExpr
                evalSizeExpr :: SizeExpr -> IO Int
                evalSizeExpr (SizeGlobalX p) =
                  case clscGlobal clsc of
                    NDRNull -> argErrorAt p "cannot evaluate global X on null range"
                    NDR1D x -> return x
                    NDR2D x y -> return x
                    NDR3D x y z -> return x
                evalSizeExpr (SizeGlobalY p) =
                  case clscGlobal clsc of
                    NDRNull -> argErrorAt p "cannot evaluate global X on null range"
                    NDR1D _ -> argErrorAt p "cannot evaluate global Y on 1D range"
                    NDR2D _ y -> return y
                    NDR3D _ y _ -> return y
                evalSizeExpr (SizeGlobalZ p) =
                  case clscGlobal clsc of
                    NDRNull -> argErrorAt p "cannot evaluate global X on null range"
                    NDR1D _ -> argErrorAt p "cannot evaluate global Y on 1D range"
                    NDR2D _ _ -> argErrorAt p "cannot evaluate global Z on 2D range"
                    NDR3D _ _ z -> return z


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