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
  info    <- parseInfo
  _       <- lexSymbol "("
  exprs   <- Mega.many sexpr
  _       <- lexSymbol ")"
  return $ S.Expr (S.Lst exprs) info

symbol :: Parser S.Expr
symbol = do
  info  <- parseInfo
  i     <- initial
  sub   <- Mega.many subsequent
  case (i:sub) of
    "::"    -> return $ S.Expr (S.SFrm S.Cons) info
    "="     -> return $ S.Expr (S.SFrm S.Def) info
    "=="    -> return $ S.Expr (S.SFrm S.IsEq) info
    "atom?" -> return $ S.Expr (S.SFrm S.IsAtm) info
    "first" -> return $ S.Expr (S.SFrm S.First) info
    "fn"    -> return $ S.Expr (S.SFrm S.Lambda) info
    "if"    -> return $ S.Expr (S.SFrm S.If) info
    "quote" -> return $ S.Expr (S.SFrm S.Quote) info
    "rest"  -> return $ S.Expr (S.SFrm S.Rest) info
    _       -> return $ S.Expr (S.Sym $ i:sub) info
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

parseInfo :: Parser S.Info
parseInfo = Mega.getParserState >>= return . stateToInfo

stateToInfo :: Mega.State s -> S.Info
stateToInfo s = S.Info (col pos) (line pos) (Pos.sourceName pos)
  where pos   = Mega.pstateSourcePos . Mega.statePosState $ s
        col   = Pos.unPos . Pos.sourceColumn
        line  = Pos.unPos . Pos.sourceLine
