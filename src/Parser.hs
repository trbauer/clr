module Parser(
    P
  , USt, dftUSt
  , parse
  , withPos
  , spToPos

  , testP

  , pTrace, pTraceLA, pTraceLAK
  , pWarningAt

  , pKeyword
  , pOneOf

  , pInt
  , pInt64
  , pDouble
  , pIdentifier
  , pLiteral
  , pTryLiteral
  , pSymbol
  , pString
  , pLexeme
  , pWhiteSpace
  , slashStarComment
  , eolComment

  ) where

import Types

import Control.Applicative hiding ((<|>))
import Control.Monad
import Data.Functor.Identity
import Data.List
import Data.Int
import Debug.Trace

import Text.Parsec.Prim((<|>),(<?>))
import qualified Text.Parsec.Prim       as P
import qualified Text.Parsec.Error      as P
import qualified Text.Parsec.Pos        as P
import qualified Text.Parsec.Combinator as P
import qualified Text.Parsec.Char       as P

type P a = P.ParsecT [Char] USt Identity a

-- type P a = P.Parser USt Char a

data USt = USt {
    uPtrSizeBytes :: !Int
  , uWs :: [Diag]
  } deriving Show
dftUSt :: USt
dftUSt =
  USt {
    uPtrSizeBytes = 4
  , uWs = []
  }

parse :: P a -> FilePath -> String -> Either Diag (a,[Diag])
parse p fp inp = body
  where mea = P.runParserT pP dftUSt fp inp -- Identity (Either ParseError (a,[Diag]))
        body =
          case runIdentity mea of
            Left pe -> Left $ Diag sp msg
              where sp = spToPos (P.errorPos pe)
                    msg = show pe -- intercalate "; " (map P.messageString (P.errorMessages pe))
            Right (a,ws) -> Right (a,ws)

        pP = do
          pWhiteSpace
          a <- p
          st <- P.getState
          P.eof
          return (a,uWs st)

withPos :: (Pos -> P a) -> P a
withPos p = do
  sp <- P.getPosition
  p (spToPos sp)

testP :: Show a => P a -> String -> IO ()
testP p inp =
  case parse p "<stdin>" inp of
    Left pe -> putStrLn $ fmtDiagWithLines (lines inp) pe
    Right (a,ws) -> do
      mapM (putStrLn . fmtDiagWithLines (lines inp)) ws
      print a

spToPos :: P.SourcePos -> Pos
spToPos sp = newPos (P.sourceLine sp) (P.sourceColumn sp)

pTrace :: String -> P ()
pTrace str = trace str (return ())
pTraceLA :: String -> P ()
pTraceLA = pTraceLAK 16
pTraceLAK :: Int -> String -> P ()
pTraceLAK k str = do
  la <- P.getInput
  pTrace $ "=> " ++ str ++ "\n" ++ show (take k la)

pWarningAt :: Pos -> String -> P ()
pWarningAt at msg = do
  u <- P.getState
  P.setState (u{uWs = Diag at msg : uWs u})

pKeyword :: String -> P ()
pKeyword kw = P.try (P.string kw >> P.notFollowedBy P.alphaNum) >> pWhiteSpace

pOneOf :: String -> [(String,a)] -> P a
pOneOf ann vs = go vs <?> ann
  where go [(s,v)] = pString s >> return v
        go ((s,v):vs) = P.try (pString s >> return v) <|> go vs


pInt :: P Int
pInt = read <$> (P.try pHex <|> pDec)
  where pHex = do
          P.char '0'
          P.char 'x'
          str <- P.many1 P.hexDigit
          return ("0x"++str)
        pDec = P.many1 P.digit

pInt64 :: P Int64
pInt64 = read <$> (P.try pHex <|> pDec)
  where pHex = do
          P.char '0'
          P.char 'x'
          str <- P.many1 P.hexDigit
          return ("0x"++str)
        pDec = P.many1 P.digit

pDouble :: P Double
pDouble = pNan <|> pInf <|> pDbl
  where pNan = pSymbol "nan" >> return (0/0)
        pInf = pSymbol "inf" >> return (1/0)

        -- examples:
        -- "3.141"
        -- "3.141e-5"
        -- "3.141e66"
        pDbl = withPos $ \p -> do
          whole <- P.many1 P.digit
          P.char '.'
          frac <- P.many P.digit
          let pExp = do
               P.oneOf "eE"
               sign <- P.option "" (P.oneOf "-+" >>= \c -> return [c])
               val <- P.many1 P.digit
               return $ "e" ++ sign ++ val
          exp <- P.option "" pExp
          return $ read $ whole ++ "." ++ frac ++ exp


pIdentifier :: P String
pIdentifier = pLexeme $ do
 c <- (P.letter <|> P.char '_')
 cs <- P.many (P.alphaNum <|> P.char '_')
 return (c:cs)

pLiteral :: String -> P ()
pLiteral str = P.string str >> pWhiteSpace
pTryLiteral :: String -> P ()
pTryLiteral = P.try . pLiteral
pSymbol :: String -> P ()
pSymbol = pLexeme . pString
pString :: String -> P ()
pString s = P.string s >> return ()

pWhiteSpace :: P ()
pWhiteSpace =
  P.many ((P.space>>return()) <|> slashStarComment <|> eolComment) >> return ()

pLexeme :: P a -> P a
pLexeme = (<* pWhiteSpace)

slashStarComment :: P ()
slashStarComment = withPos $ \pos -> pTryLiteral "/*" >> slashStarBegin pos
  where slashStarBegin pos = slashStarBody
          where slashStarBody = pTryLiteral "*/" <|> slashStarBadEof <|> (P.anyChar >> slashStarBody)
                slashStarBadEof = do
                  P.eof
                  fail ("unexpected EOF in /* comment (which started at " ++ fmtPos pos ++ ")")

eolComment :: P ()
eolComment = pLiteral "//" >> P.manyTill P.anyChar end >> return ()
  where end = (P.char '\n' >> return ()) <|> P.eof
