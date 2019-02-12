module Parse (parseFile, parseStr) where

import Control.Applicative ((<|>), empty)
import Control.Monad.Combinators (between)
import qualified Text.Megaparsec.Char as Char
import qualified Text.Megaparsec.Char.Lexer as Lex
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Error as Err
import qualified Text.Megaparsec.Pos as Pos
import Data.Void (Void)
import qualified Syntax as S

type Parser = Mega.Parsec Void String

type ParseError = Err.ParseErrorBundle String Void

parseFile :: String -> String -> Either ParseError [S.Expr]
parseFile filename = Mega.parse (contents program) filename

parseStr :: String -> Either ParseError S.Expr
parseStr = Mega.parse (contents sexpr) ""

contents :: Parser a -> Parser a
contents p = between space Mega.eof p

program :: Parser [S.Expr]
program = Mega.sepBy sexpr space

sexpr :: Parser S.Expr
sexpr = lexeme (atom <|> list)

atom :: Parser S.Expr
atom = Mega.label "atom" symbol

list :: Parser S.Expr
list = Mega.label "list" $ do
  s       <- Mega.getParserState
  let info = stateToInfo s
  _       <- lexSymbol "("
  exprs   <- Mega.many sexpr
  _       <- lexSymbol ")"
  return $ S.Expr (S.Lst exprs) info

symbol :: Parser S.Expr
symbol = do
  s     <- Mega.getParserState
  let info = stateToInfo s
  let toExpr x = S.Expr x info
  i   <- initial
  sub <- Mega.many subsequent
  case (i:sub) of
    "::"    -> return $ toExpr $ S.SFrm S.Cons
    "="     -> return $ toExpr $ S.SFrm S.Def
    "=="    -> return $ toExpr $ S.SFrm S.IsEq
    "atom?" -> return $ toExpr $ S.SFrm S.IsAtm
    "first" -> return $ toExpr $ S.SFrm S.First
    "fn"    -> return $ toExpr $ S.SFrm S.Lambda
    "if"    -> return $ toExpr $ S.SFrm S.If
    "quote" -> return $ toExpr $ S.SFrm S.Quote
    "rest"  -> return $ toExpr $ S.SFrm S.Rest
    _       -> return $ toExpr $ S.Sym (i:sub)
  where
      initial     = Char.letterChar <|> Mega.oneOf "!$%&*/:<=>?~_^"
      subsequent  = initial <|> Char.digitChar <|> Mega.oneOf ".+-"

space :: Parser ()
space = Lex.space Char.space1 lineComment empty
  where
    lineComment :: Parser ()
    lineComment = Mega.label "comment" $ Lex.skipLineComment ";"

lexeme :: Parser a -> Parser a
lexeme = Lex.lexeme space

lexSymbol :: String -> Parser String
lexSymbol = Lex.symbol space

stateToInfo :: Mega.State s -> S.Info
stateToInfo s = S.Info (col pos) (line pos) (Pos.sourceName pos)
  where pos = Mega.pstateSourcePos . Mega.statePosState $ s
        col = Pos.unPos . Pos.sourceColumn
        line = Pos.unPos . Pos.sourceLine
