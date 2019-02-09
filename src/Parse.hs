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
atom = Mega.label "atom" (specialForm <|> symbol)

list :: Parser S.Expr
list = Mega.label "list" $ do
  s       <- Mega.getParserState
  let info = stateToInfo s
  _       <- lexSymbol "("
  exprs   <- Mega.many sexpr
  _       <- lexSymbol ")"
  return $ S.Expr (S.Lst exprs) info

specialForm :: Parser S.Expr
specialForm = do
  s     <- Mega.getParserState
  let info = stateToInfo s
  expr  <- S.SFrm <$> (car <|> cdr <|> cons <|> cond <|> def <|> isAtm <|> isEq <|> lambda <|> quote)
  return $ S.Expr expr info

car :: Parser S.SpecialForm
car = Char.string "car" >> return S.Car

cdr :: Parser S.SpecialForm
cdr = Char.string "cdr" >> return S.Cdr

cons :: Parser S.SpecialForm
cons = Char.string "cons" >> return S.Cons

cond :: Parser S.SpecialForm
cond = Char.string "cond" >> return S.Cond

def :: Parser S.SpecialForm
def = Char.string "define" >> return S.Def

isAtm :: Parser S.SpecialForm
isAtm = Char.string "atom?" >> return S.IsAtm

isEq :: Parser S.SpecialForm
isEq = Char.string "eq?" >> return S.IsEq

lambda :: Parser S.SpecialForm
lambda = Char.string "lambda" >> return S.Lambda

quote :: Parser S.SpecialForm
quote = Char.string "quote" >> return S.Quote

symbol :: Parser S.Expr
symbol = do
  s     <- Mega.getParserState
  let info = stateToInfo s
  i   <- initial
  sub <- Mega.many subsequent
  return $ S.Expr (S.Sym $ i:sub) info
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
