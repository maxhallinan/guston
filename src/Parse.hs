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

parseFile :: String -> String -> Either ParseError S.Sexpr
parseFile filename = Mega.parse (contents program) filename

parseStr :: String -> Either ParseError S.Sexpr
parseStr = Mega.parse (contents sexpr) ""

contents :: Parser a -> Parser a
contents p = between space Mega.eof p

program :: Parser S.Sexpr
program = S.Lst <$> Mega.sepBy sexpr space

sexpr :: Parser S.Sexpr
sexpr = lexeme (atom <|> list)

atom :: Parser S.Sexpr
atom = specialForm <|> symbol

list :: Parser S.Sexpr
list = do
  _     <- lexSymbol "("
  exprs <- Mega.many sexpr
  _     <- lexSymbol ")"
  return $ S.Lst exprs

specialForm :: Parser S.Sexpr
specialForm = S.SFrm <$> (car <|> cdr <|> cons <|> cond <|> def <|> isAtom <|> isEq <|> lambda <|> quote)

car :: Parser S.SpecialForm
car = Char.string "car" >> return S.Car

cdr :: Parser S.SpecialForm
cdr = Char.string "cdr" >> return S.Cdr

cons :: Parser S.SpecialForm
cons = Char.string "cons" >> return S.Cns

cond :: Parser S.SpecialForm
cond = Char.string "cond" >> return S.Cond

def :: Parser S.SpecialForm
def = Char.string "define" >> return S.Def

isAtom :: Parser S.SpecialForm
isAtom = Char.string "atom?" >> return S.IsAtm

isEq :: Parser S.SpecialForm
isEq = Char.string "eq?" >> return S.IsEq

lambda :: Parser S.SpecialForm
lambda = Char.string "lambda" >> return S.Lambda

quote :: Parser S.SpecialForm
quote = Char.string "quote" >> return S.Quot

symbol :: Parser S.Sexpr
symbol = do
  i   <- initial
  sub <- Mega.many subsequent
  return $ S.Sym (i : sub)
  where
      initial     = Char.letterChar <|> Mega.oneOf "!$%&*/:<=>?~_^"
      subsequent  = initial <|> Char.digitChar <|> Mega.oneOf ".+-"

space :: Parser ()
space = Lex.space Char.space1 lineComment empty
  where
    lineComment :: Parser ()
    lineComment = Lex.skipLineComment ";"

lexeme :: Parser a -> Parser a
lexeme = Lex.lexeme space

lexSymbol :: String -> Parser String
lexSymbol = Lex.symbol space
