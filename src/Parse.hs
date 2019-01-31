module Parse (parseFile, parseString) where

import Control.Applicative ((<|>))
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

parseString :: String -> Either ParseError S.Sexpr
parseString = Mega.parse (contents sexpr) ""

contents :: Parser a -> Parser a
contents p = between space Mega.eof p

program :: Parser S.Sexpr
program = S.List <$> Mega.sepBy sexpr space

sexpr :: Parser S.Sexpr
sexpr = lexeme atom

atom :: Parser S.Sexpr
atom = symbol

symbol :: Parser S.Sexpr
symbol = do
  i   <- initial 
  sub <- Mega.many subsequent
  return $ S.Symbol (i : sub)
  where 
      initial     = Char.letterChar <|> Mega.oneOf "!$%&*/:<=>?~_^" 
      subsequent  = initial <|> Char.digitChar <|> Mega.oneOf ".+-"

space :: Parser ()
space = Lex.space Char.space1 lineComment blockComment
  where
    lineComment :: Parser ()
    lineComment = Lex.skipLineComment ";"

    blockComment :: Parser ()
    blockComment = Lex.skipBlockComment "{-" "-}"

lexeme :: Parser a -> Parser a
lexeme = Lex.lexeme space
