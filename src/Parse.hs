module Parse (parseFile, parseStr) where

import Control.Applicative ((<|>), empty)
import Control.Monad.Combinators (between)
import qualified Text.Megaparsec.Char as Char
import qualified Text.Megaparsec.Char.Lexer as Lex
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Error as Err
import Data.Void (Void)
import qualified Syntax as S

type Parser = Mega.Parsec Void String

type ParseError = Err.ParseErrorBundle String Void

parseFile :: String -> String -> Either ParseError [S.XExpr]
parseFile filename = Mega.parse (contents program) filename

parseStr :: String -> Either ParseError S.XExpr
parseStr = Mega.parse (contents xexpr) ""

contents :: Parser a -> Parser a
contents p = between space Mega.eof p

program :: Parser [S.XExpr]
program = Mega.sepBy xexpr space

xexpr :: Parser S.XExpr
xexpr = lexeme (withInfo $ atom <|> list)

withInfo :: Parser S.Expr -> Parser S.XExpr
withInfo parseExpr = do
  start <- Mega.getOffset
  expr  <- parseExpr
  end   <- Mega.getOffset
  return $ S.XExpr expr (S.Info (start, end))

atom :: Parser S.Expr
atom = Mega.label "atom" symbol

list :: Parser S.Expr
list = Mega.label "list" $ do
  _       <- lexSymbol "("
  exprs   <- Mega.many xexpr
  _       <- lexSymbol ")"
  return $ S.Lst exprs

symbol :: Parser S.Expr
symbol = do
  i     <- initial
  sub   <- Mega.many subsequent
  case (i:sub) of
    "::"    -> return $ S.SFrm S.Cons
    "="     -> return $ S.SFrm S.Def
    "=="    -> return $ S.SFrm S.IsEq
    "atom?" -> return $ S.SFrm S.IsAtm
    "first" -> return $ S.SFrm S.First
    "fn"    -> return $ S.SFrm S.Lambda
    "if"    -> return $ S.SFrm S.If
    "quote" -> return $ S.SFrm S.Quote
    "rest"  -> return $ S.SFrm S.Rest
    _       -> return $ S.Sym (i:sub)
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

