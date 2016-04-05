{-# LANGUAGE FlexibleContexts #-}
module CLParser(
    module Parser
  , pFindKernels
  ) where

import Types
import BaseParser

import Control.Applicative hiding ((<|>))
import Control.Monad
import Data.Functor.Identity
import Data.List
import Debug.Trace

import Text.Parsec.Prim((<|>),(<?>))
import qualified Text.Parsec.Prim       as P
import qualified Text.Parsec.Error      as P
import qualified Text.Parsec.Pos        as P
import qualified Text.Parsec.Combinator as P
import qualified Text.Parsec.Char       as P


pFindKernels :: [Kern] -> P [Kern]
pFindKernels rks = pK <|> pOtherChar <|> (P.eof >> return (reverse rks))
  where pOtherChar = P.anyChar >> pFindKernels rks
        pK = do
          k <- pKernel
          pFindKernels (k:rks)

pKernel :: P Kern
pKernel = withPos $ \p -> do
 as0 <- P.many pAttr
 pKernelKeyword
 as1 <- P.many pAttr
 pLiteral "void"
 as2 <- P.many pAttr
 knm <- pIdentifier
 pLiteral "("
 args <- P.sepBy pParam (pSymbol ",")
 pLiteral ")"
 b <- pKernelBody p
 return $!
  Kern {
    kName = knm
  , kParams = args
  , kAttrs = as0 ++ as1 ++ as2
  , kBody = b
  }

-- __attribute__((reqd_work_group_size(8, 1, 1)))
pAttr :: P KAttr
pAttr = body
  where body = do
          pKeyword "__attribute__"
          pSymbol "("
          a <- pAttrBody
          pSymbol ")"
          return a
        pAttrBody :: P KAttr
        pAttrBody =
              P.try (pSymAttrBody "reqd_work_group_size" KAReqWgSize)
          <|> P.try (pSymAttrBody "work_group_size_hint" KAHintWgSize)
          <|> P.try (pSymAttrBody "reqd_sub_group_size" KAReqSgSize)
          <|> pRawAttr

        pSymAttrBody :: String -> (Int -> Int -> Int -> KAttr) -> P KAttr
        pSymAttrBody sym cons = do
          pSymbol "("
          pKeyword sym
          pSymbol "("
          x <- pInt
          pSymbol ","
          y <- pInt
          pSymbol ","
          z <- pInt
          pSymbol ")"
          pSymbol ")"
          return $ cons x y z

        pRawAttr :: P KAttr
        pRawAttr = do
          pSymbol "("
          str <- P.manyTill P.anyChar (P.lookAhead (P.char ')'))
          pSymbol ")"
          return $ KAUnknown str

pParam :: P KParam
pParam = do
  pMods -- const|volatile
  q <- P.option KQualPrivate (P.try pKQual) -- global|local|constant
  pMods -- const|volatile
  a <- pKAcc -- read_only|write_only|...
  ty <- pType
  nm <- pIdentifier
  return $! KParam nm q a ty

pMods :: P ()
pMods = P.many pMod >> return ()
pMod :: P ()
pMod =
      P.try (pKeyword "const")
  <|> P.try (pKeyword "volatile")
  <|> P.try (pKeyword "register")
  <|> pKeyword "restrict"


pKernelKeyword :: P ()
pKernelKeyword = pLiteral "__kernel" <|>   pLiteral "kernel"

pKQual :: P KQual
pKQual =  tl KQualGlobal "global" "__global"
      <|> tl KQualLocal "local" "__local"
      <|> tl KQualConstant "constant" "__constant"
      <|> tl KQualPrivate "private" "__private"
  where tl val s1 s2 = (pTryLiteral s1 <|> pTryLiteral s2) >> return val

pKAcc :: P KAcc
pKAcc = tl KAccReadOnly "read_only"
    <|> tl KAccWriteOnly "write_only"
    <|> tl KAccReadWriteOnly "read_write"
    <|> return KAccNone
  where tl v sym = pTryLiteral sym >> return v

pType :: P Type
pType = body
  where body = P.try pBuiltinType <|> pPtrOrScalar

        -- T '*'?
        pPtrOrScalar = do
          let pPtrSuffix ty = pSymbol "*" >> return (PointerType ty)
          ty <- pBaseType
          P.try (pPtrSuffix ty) <|> return ty

        pBaseType =
              P.try pVectorOrScalarType
          <|> P.try pNonVectorType
          <|> P.try pVoid
          <|> P.try pBoolType
          <|> P.try pSignedPtrInt
          <|> pBuiltinType
        pVoid = pKeyword "void" >> return VoidType
        pBoolType = pKeyword "bool" >> return BoolType
        pVectorOrScalarType = do
          t <- pVectBaseType
          v <- P.option 0 (P.try (pOneOf "" [("2",2),("3",3),("4",4),("8",8),("16",16)]))
          pWhiteSpace
          return $ t {tElems = v}

        pSignedPtrInt = do
          sz <- uPtrSizeBytes <$> P.getState
          pOneOf "type" [
              ("size_t",PrimType TGroupSInt 1 sz)
            , ("ptrdiff_t",PrimType TGroupSInt 1 sz)
            , ("intptr_t",PrimType TGroupSInt 1 sz)
            , ("uintptr_t",PrimType TGroupUInt 1 sz)
            ]
        pVectBaseType =
          pOneOf "type" [
            ("char",PrimType TGroupSInt 1 1)
          , ("uchar",PrimType TGroupUInt 1 1)
          , ("short",PrimType TGroupSInt 1 2)
          , ("ushort",PrimType TGroupUInt 1 2)
          , ("int",PrimType TGroupSInt 1 4)
          , ("uint",PrimType TGroupUInt 1 4)
          , ("long",PrimType TGroupSInt 1 8)
          , ("ulong",PrimType TGroupUInt 1 8)
          , ("half",PrimType TGroupFlt 1 2)
          , ("float",PrimType TGroupFlt 1 4)
          , ("double",PrimType TGroupFlt 1 8)
          ]
        -- types that may only appear as non-vector form
        -- signed int
        pNonVectorType =
          P.try pUnsignedType <|> pSignedType
        pUnsignedType = do
          pKeyword "unsigned"
          t <- pPrimInt
          pWhiteSpace
          return $ t{tGroup = TGroupUInt}
        pSignedType = do
          pKeyword "signed"
          t <- pPrimInt
          pWhiteSpace
          return $ t{tGroup = TGroupSInt}
        pPrimInt =
          pOneOf "type" [
            ("char",PrimType TGroupSInt 1 1)
          , ("short",PrimType TGroupSInt 1 2)
          , ("int",PrimType TGroupSInt 1 4)
          , ("long",PrimType TGroupSInt 1 8)
          ]

        pBuiltinType = pLexeme $
          pOneOf "type" [
            ("image1d_t",ImageType1DType)
          , ("image2d_t",ImageType2DType)
          , ("image3d_t",ImageType3DType)
          , ("image1d_array_t",ImageType1DArrayType)
          , ("image2d_array_t",ImageType2DArrayType)
          , ("sampler_t",SamplerType)
          , ("event_t",EventType)
          ]

pKernelBody :: Pos -> P String
pKernelBody p = body <?> "kernel body"
  where body = do
         -- pTraceLAK 16 $ "pKernelBody"
         P.char '{'
         str <- go [] 0
         pWhiteSpace
         return $ "{" ++ str ++ "}"

        go :: [String] -> Int -> P String
        go rstrs n = goKb
          where goKb = do
                  -- pTraceLAK 16 ("go "++(concat (reverse rstrs)) ++ " " ++ show n)
                  goElem
                goElem =
                      P.try pOpenBrace
                  <|> P.try pCloseBrace
                  <|> P.try pEolComm
                  <|> P.try pStrLit
                  <|> P.try pSlashStarComm
                  <|> P.try pSpaces
                  <|> P.try pEarlyEof
                  <|> pOtherChar
                pStrLit :: P String
                pStrLit = do
                  P.char '"'
                  s <- P.manyTill pStrLitElem (P.char '"')
                  continue ("\"" ++ s ++ "\"")

                pStrLitElem =
                      P.try (P.char '\\' >> pEsc) <|> P.anyChar
                  where pEsc =
                              (P.char '\\' >> return '\\')
                          <|> (P.char '"' >> return '\"')
                          <|> (P.char '\'' >> return '\'')
                          <|> (P.char 'n' >> return '\n')
                          <|> (P.char 't' >> return '\t')
                          <|> (P.char 'v' >> return '\v')
                          <|> (P.char 'a' >> return '\a')
                          <|> (P.char 'r' >> return '\r')

                pOpenBrace :: P String
                pOpenBrace = do
                  P.char '{'
                  go ("{":rstrs) (n+1)
                pCloseBrace :: P String
                pCloseBrace = do
                  P.char '}'
                  if n == 0 then return (reverse (concat rstrs) ++ "}")
                    else go ("}":rstrs) (n-1)
                pSpaces :: P String
                pSpaces = P.many1 P.space >>= continue
                pOtherChar :: P String
                pOtherChar = do
                  c <- P.noneOf "{}"
                  continue [c]
                pEarlyEof :: P String
                pEarlyEof = do
                  P.eof
                  fail ("cannot find end of kernel starting at " ++ fmtPos p)
                pEolComm :: P String
                pEolComm = body
                  where body = do
                          P.char '/' >> P.char '/'
                          s <- P.manyTill P.anyChar end
                          continue $ "//" ++ s ++ "\n"

                        end = P.char '\n'

                pSlashStarComm = do
                  P.char '/' >> P.char '*'
                  str <- P.manyTill P.anyChar (P.string "*/")
                  continue ("/*" ++ str)

                continue str =
                  go (str:rstrs) n


