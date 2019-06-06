module Parser where

import Data.Void
import Text.Megaparsec
import Control.Monad.Combinators.Expr
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

integer :: Parser Int
integer = lexeme L.decimal

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

keyword :: String -> Parser ()
keyword s = (lexeme . try) (C.string s *> notFollowedBy C.alphaNumChar)

-- the actual parser
identifier :: Parser Ident
identifier = lexeme $ (:) <$> C.letterChar <*> many C.alphaNumChar

-- expression parser
lambda :: Parser Expr
lambda = do
  keyword "\\"
  param <- identifier
  keyword "=>"
  body <- expr
  return $ Lambda param body

operators :: [[Operator Parser Expr]]
operators =
  [ [ InfixL (Apply <$ symbol "") ] ]

term :: Parser Expr
term = parens expr
  <|> lambda
  <|> Variable <$> identifier
  <|> Integer <$> integer

expr :: Parser Expr
expr = makeExprParser term operators

-- wrap them up
parser :: Parser Expr
parser = between space eof expr
