{-# LANGUAGE FlexibleContexts #-}
module CLSParser(
    module Parser
  , pCLSSts
  ) where

import Control.Applicative hiding ((<|>))
import Types
import Parser

import Data.Int
import Text.Parsec.Prim((<|>),(<?>))
import qualified Text.Parsec.Prim       as P
import qualified Text.Parsec.Combinator as P
import qualified Text.Parsec.Char       as P

-- dir/file.cl`kernel[-DT=int]<4096x4096>(0:w,0)
pCLSSts :: P [CLSSt]
pCLSSts = P.sepBy1 pCLSSt pDelim
  where pDelim = P.option () (pSymbol ";" >> return ())

pCLSSt :: P CLSSt
pCLSSt = pLetSt <|> pCallSt
  where -- let x = ..., y = ...
        pLetSt = withPos $ \p -> do
          pKeyword "let"
          bs <- pBindings
          return $! CLSStLet p bs

        pCallSt = withPos $ \p -> do
          c <- pCLSCall
          -- bs <- P.option [] $ pKeyword "where" >> pBindings
          return $! CLSStCall p c -- bs

        pBindings :: P [(String,Init)]
        pBindings = P.sepBy1 (P.try pBinding) (pSymbol ",")
          where pBinding = do
                  nm <- pIdentifier
                  pSymbol "="
                  arg <- pKernelArg
                  return (nm,arg)


pCLSCall :: P CLSCall
pCLSCall = withPos $ \p -> do
  pfx <- P.option "" (P.try pFilePathPrefix)
  knm <- pIdentifier
  bopts <- P.option "" (P.try pBuildOptions)
  (glb,loc) <- pRange
  P.char '('
  as <- P.sepBy pKernelArg (pSymbol ",")
  P.char ')'
  return $!
    CLSCall {
      clscPos = p
    , clscPath = pfx
    , clscKernel = knm
    , clscBuildOpts = bopts
    , clscGlobal = glb
    , clscLocal = loc
    , clscArgs = as
    }


-- dir/file.cl`
pFilePathPrefix :: P FilePath
pFilePathPrefix = pComponent
  where pComponent :: P FilePath
        pComponent = do
          let pPathChar :: P Char
              pPathChar = P.alphaNum <|> P.oneOf ".,[]+-_@"
          p <- P.many1 pPathChar
          ps <- pEnd <|> pMorePath
          return $ p ++ ps
        pEnd :: P FilePath
        pEnd = P.char '`' >> return ""
        pMorePath :: P FilePath
        pMorePath = do
          P.char '/' <|> P.char '\\'
          p <- pComponent
          return $ "/" ++ p

-- [-DT=int -I includes -cl-opt-disable]
pBuildOptions :: P String
pBuildOptions = body
  where body = do
          pSymbol "["
          str <- P.many (P.try pBuildOptEsc <|> pBuildOptChar)
          pSymbol "]"
          return str

        pBuildOptEsc = P.char '\\' >> (P.char ']' <|> P.char '\\')

        pBuildOptChar = P.noneOf "]"

-- rename NDRange to Size or Dim?
--
-- dir/file.cl`kernel<4096x4096>(0:w,0)
--                   ^^^^^^^^^^^
pRange :: P (NDRange,NDRange)
pRange = do
  pSymbol "<"
  glb <- pNDRange
  loc <- P.option NDRNull $ do
           pSymbol ","
           pNDRange
  pSymbol ">"
  return (glb,loc)

-- 4096x4096 or 4096 or
pNDRange :: P NDRange
pNDRange = p1D
  where p1D = do
          x <- pInt
          P.try (pSymbol "x" >> p2D x) <|> return (NDR1D x)
        p2D x = do
          y <- pInt
          P.try (pSymbol "x" >> p3D x y) <|> return (NDR2D x y)
        p3D x y = do
          z <- pInt
          return (NDR3D x y z)




-- <val> (':' bsize? battrs)?
--
-- 0:w
-- img("boo.bmp"):r
-- 15
-- 0x123
-- {1,2,3,4}
pKernelArg :: P Init
pKernelArg = body
  where body = withPos $ \p ->do
          a <- pScaArg
          P.option a $ do
            P.char ':'
            msz <- P.optionMaybe pSizeExpr
            let pFinishBuf1 = do
                   acc <- pBufAcc
                   bt <- pBufTrans
                   return $! InitBuf p a msz acc bt
                pFinishBuf2 = do
                   acc <- pBufAcc
                   bt <- pBufTrans
                   return $! InitBuf p a msz acc bt
            P.try pFinishBuf1 <|> pFinishBuf2

        pSizeExpr :: P SizeExpr
        pSizeExpr = do
          pSymbol "["
          e <- pSizeExprBody
          pSymbol "]"
          return e

        pBufAcc = body <?> "buffer access: 'rw', 'r', or 'w'"
          where body =  P.try ((P.char 'r' >> P.char 'w') >> return BufAccRW)
                    <|> P.try ((P.char 'w' >> P.char 'r') >> return BufAccRW)
                    <|> (P.char 'r' >> return BufAccR)
                    <|> (P.char 'w' >> return BufAccW)

        pBufTrans = body <?> "transfer method: 'c', 'm', or 's'"
          where body = (P.char 'c' >> return BufTransC)
                    <|> (P.char 'm' >> return BufTransM)
                    <|> (P.char 's' >> return BufTransS)
                    <|> return BufTransC


-- [8*gx]
pSizeExprBody :: P SizeExpr
pSizeExprBody = pSizeExprAddSub
  where pSizeExprAddSub = withPos $ \p -> do
          e1 <- pSizeExprMulDivMod
          P.option e1 $ P.try $ do
            cons <- pAddOp
            e2 <- pSizeExprAddSub
            return $! cons p e1 e2

        pAddOp =
              P.try (pSymbol "+" >> return SizeAdd)
          <|> (pSymbol "-" >> return SizeSub)

        pSizeExprMulDivMod = withPos $ \p -> do
          e1 <- pPrimary
          P.option e1 $ P.try $ do
            cons <- pMulOp
            e2 <- pSizeExprMulDivMod
            return $! cons p e1 e2

        pMulOp =
              P.try (pSymbol "*" >> return SizeMul)
          <|> P.try (pSymbol "/" >> return SizeDiv)
          <|> (pSymbol "%" >> return SizeMod)

        pPrimary = pIntSize <|> pDimSize <|> pGrp <|> pSizeRef

        pDimSize = pGlb <|> pLoc
          where pGlb = withPos $ \p -> do
                  P.char 'g' >> ((P.char 'x' >> return (SizeGlobalX p))
                                  <|> (P.char 'y' >> return (SizeGlobalY p))
                                  <|> (P.char 'z' >> return (SizeGlobalZ p)))
                pLoc = withPos $ \p -> do
                  P.char 'l' >> ((P.char 'x' >> return (SizeLocalX p))
                                  <|> (P.char 'y' >> return (SizeLocalY p))
                                  <|> (P.char 'z' >> return (SizeLocalZ p)))

        pSizeRef = withPos $ \p -> do
          id <- pIdentifier
          return $! SizeRef p id


        pIntSize = withPos $ \p -> SizeLit p <$> pScaled pInt64

        pGrp = do
          pSymbol "("
          e <- pSizeExprAddSub
          pSymbol ")"
          return e

pScaled :: Num a => P a -> P a
pScaled par = do
  z <- par
  let pK = P.oneOf "kK" >> return (1000*z)
      pM = P.oneOf "mM" >> return (1000*1000*z)
      pB = P.oneOf "bB" >> return (1000*1000*1000*z)
  P.option z (pK <|> pM <|> pB)

pScaArg :: P Init
pScaArg = pRecord <|> P.try pFloating <|> pIntegral <|> pReference
  where pRecord = withPos $ \p -> do
          pSymbol "{"
          as <- P.sepBy1 pScaArg (pSymbol ",")
          pSymbol "}"
          return $ InitRec p as

        pIntegral = withPos $ \p -> do
          f <- P.option id (pSymbol "-" >> return negate)
          (InitInt p . f) <$> pScaled pInt64

        pFloating = withPos $ \p -> do
          f <- P.option id (pSymbol "-" >> return negate)
          (InitFlt p . f) <$> pScaled pDouble

        pReference = withPos $ \p -> do
          InitRef p <$> pIdentifier
