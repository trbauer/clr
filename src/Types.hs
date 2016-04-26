module Types where

import Data.Int


fmtKern :: Kern -> String
fmtKern k =
  "Kern {\n" ++
  "  kName = " ++ show (kName k) ++ "\n" ++
  ", kParams = [\n" ++
  concatMap (("  "++) . (++"\n") . show) (kParams k) ++
  "  ]" ++
  ", kAttrs = " ++ show (kAttrs k) ++ "\n" ++
  ", kBody = <<\n" ++
  kBody k ++ "\n" ++
  ">>\n" ++
  "}"

data Kern =
  Kern {
    kName :: !String
  , kParams :: ![KParam]
  , kAttrs :: [KAttr]
  , kBody :: !String
  } deriving Show

data KParam = KParam {
    kpName :: !String
  , kpQual :: !KQual -- global/local/constant/private
  , kpAcc :: !KAcc -- read_only, write_only, read_write, none
  , kpType :: !Type -- int4, int4*, ... etc..
  } deriving Show

data KQual =
    KQualGlobal
  | KQualLocal
  | KQualConstant
  | KQualPrivate
  deriving (Show,Eq)
data KAcc =
    KAccReadOnly
  | KAccWriteOnly
  | KAccReadWriteOnly
  | KAccNone
  deriving (Show,Eq)
data Type =
    PrimType {
      tGroup :: !TGroup
    , tElems :: !Int -- vector elements (e.g. float4 is 4, float is 1)
    , tElemSize :: !Int -- bytes per elem (float and float4 are both 4, short is 2)
    }
  | VoidType -- void
  | BoolType -- bool
  | PointerType !Type -- T*
  | ImageType1DType -- image1d_t
  | ImageType2DType -- image2d_t
  | ImageType3DType -- image3d_t
  | ImageType1DArrayType -- image1d_array_t
  | ImageType2DArrayType -- image2d_array_t
  | SamplerType -- sampler_t
  | EventType -- event_t
  deriving (Show,Eq)

data KAttr =
    KAReqWgSize  !Int !Int !Int -- __attribute__((reqd_work_group_size(X, Y, Z)))
  | KAHintWgSize !Int !Int !Int -- __attribute__((work_group_size_hint(X, Y, Z)))
  | KAReqSgSize  !Int !Int !Int -- __attribute__((reqd_sub_group_size(X, Y, Z)))
  | KAUnknown    !String        -- __attribute__((vec_type_hint(<typen>))) -- e.g. double2
  deriving (Show,Eq)

data TGroup =
    TGroupSInt
  | TGroupUInt
  | TGroupFlt
  | TGroupStruct !String ![(String,Type)]
  deriving (Show,Eq)

data Pos = Pos {pLine :: !Int, pCol :: !Int} deriving (Show,Eq,Ord)
data Diag = Diag {dPos :: !Pos, dMessage :: !String} deriving Show

fmtDiag :: Diag -> String
fmtDiag = fmtDiagWithLines []

fmtDiagWithLines :: [String] -> Diag -> String
fmtDiagWithLines lns d
  | p == noPos = dMessage d
  | otherwise = fmtPos p ++ ": " ++ dMessage d ++ source_info
  where p = dPos d
        source_info
          | pLine p - 1 >= length lns = ""
          | otherwise = "\n" ++
              lns !! (pLine p - 1) ++ "\n" ++
              replicate (pCol p - 1) ' ' ++ "^"

fmtPos :: Pos -> String
fmtPos p = show (pLine p) ++ "." ++ show (pCol p)

newPos :: Int -> Int -> Pos
newPos = Pos

noPos :: Pos
noPos = newPos 0 0


-- bimg@buf("img.png"):rw
-- bmp("foo.bmp",16):rw -- pads the image out
-- dir/file.cl`kernel<4096x4096>(out@0:w);
-- clFinish();
-- buf.write("foo.buf");
--
-- foo<1920x1080,16x16>(0:w,0:r,{1,2,3,4}:rw,
data CLS =
  CLS {
    clsStatements :: ![CLSSt]
  } deriving Show

data CLSSt =
    CLSStCall !Pos !CLSCall -- ![(String,Arg)] -- foo<1024>(buf,s) where buf = 0:w, s={1,2,3,4}
  | CLSStLet  !Pos ![(String,Init)] -- let b1=0:w, b2=1:r
  | CLSStProg !Pos String !FilePath !String -- let prog = foo/bar.cl[...]
  -- | CLStProg !Pos String !FilePath !String -- prog`k1(...)
  deriving Show

-- dir/file.cl`kernel[opts]<4096x4096>(out@0:w);
data CLSCall =
  CLSCall {
    clscPos :: !Pos
  , clscPath :: !FilePath -- dir/file.cl`
  , clscKernel :: !String -- kernel
  , clscBuildOpts :: !String -- [-DT=int]
  , clscGlobal :: !NDRange -- <4096x4096>
  , clscLocal :: !NDRange -- <...,16x16>
  , clscArgs :: ![Init] -- (..., ..., ...)
  } deriving Show

data NDRange =
    NDRNull
  | NDR1D !Int -- e.g. 1024 or 4k
  | NDR2D !Int !Int  -- e.g. 4096x4096
  | NDR3D !Int !Int !Int -- e.g. 256x256x3
  deriving (Show,Eq)

data Init =
    InitInt !Pos !Int64 -- e.g. "0" or "0x1234"
  | InitFlt !Pos !Double -- "3.14" or "3e-9"
  | InitRec !Pos ![Init] -- {3,4,2,1}
  | InitBuf !Pos !Init !(Maybe SizeExpr) !BufAcc !BufTrans -- 0:[4*nd.x*nd.y]rw
-- | InitBufFile !Pos (Ptr ())
-- | InitFile !Pos !FilePath -- img("foo.bmp",RGBA,16):rw ... .buf("foo.dat",16)
  | InitRef !Pos !String -- "a"
  | InitSeq !Pos [Init] -- seq(1,1)      1,2,3,4,5,...
  | InitCyc !Pos [Init] -- cyc(1,2,4)
  | InitRand !Pos !(Maybe Init) !(Maybe Init) -- rand(lo,hi), rand(hi), rand
  deriving (Show,Eq)

data SizeExpr =
-- TODO: remove these and use SizeRef?
    SizeGlobalX !Pos | SizeGlobalY !Pos | SizeGlobalZ !Pos
  | SizeLocalX  !Pos | SizeLocalY  !Pos | SizeLocalZ !Pos

  | SizeDiv !Pos !SizeExpr !SizeExpr
  | SizeMul !Pos !SizeExpr !SizeExpr
  | SizeMod !Pos !SizeExpr !SizeExpr

  | SizeSub !Pos !SizeExpr !SizeExpr
  | SizeAdd !Pos !SizeExpr !SizeExpr
--  | SizeShl !Pos !SizeExpr !SizeExpr
--  | SizeShr !Pos !SizeExpr !SizeExpr
  | SizeLit !Pos !Int64
  | SizeRef !Pos !String -- foo ... where foo = 4k
  deriving (Show,Eq)

data BufAcc =
    BufAccR -- w
  | BufAccW -- r
  | BufAccRW -- rw or wr
  deriving (Show,Eq)

data BufTrans =
    BufTransC -- c (read/write (uses read/write buffer); default is mapped)
  | BufTransM -- m (mapped)
  | BufTransS -- s (svm)
  deriving (Show,Eq)




