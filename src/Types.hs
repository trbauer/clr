module Types where

fmtKern :: Kern -> String
fmtKern k =
  "Kern {\n" ++
  "  kName = " ++ show (kName k) ++ "\n" ++
  ", kParams = [\n" ++
  concatMap (("  "++) . (++"\n") . show) (kParams k) ++
  "  ]" ++
  ", kAttrs = " ++ show (kAttrs k) ++ "\n" ++
  ", kBody = (\n" ++
  show (kBody k) ++ "\n" ++
  ")\n" ++
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
  | PointerType !Type -- T *
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


