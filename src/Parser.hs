module Parser where

import Data.Void
import Text.Megaparsec
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

-- syntax tree
type Ident = String

data Expr
  = Integer Int
  | Lambda Ident Expr
  | Apply Expr Expr
  | Variable Ident
  deriving (Show)

-- parser type definition
type Parser = Parsec Void String

-- lexer utils
space :: Parser ()
space = L.space C.space1 line block
  where
    line = L.skipLineComment "//"
    block = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

symbol :: String -> Parser String
symbol = L.symbol space

integer :: Parser Integer
integer = lexeme L.decimal

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- the actual parser
identifier :: Parser String
identifier = lexeme $ (:) <$> C.letterChar <*> many C.alphaNumChar
