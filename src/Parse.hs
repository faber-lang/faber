module Parse where

import Data.Void
import Control.Arrow
import Text.Megaparsec hiding (ParseError)
import Text.Megaparsec.Error (errorBundlePretty)
import Control.Monad.Combinators.Expr
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Operators as Op

-- syntax tree
type Ident = String

data Expr
  = Integer Int
  | Lambda [Ident] Expr
  | Apply Expr Expr
  | Variable Ident
  | Tuple [Expr]
  | BinaryOp Op.BinaryOp Expr Expr
  | SingleOp Op.SingleOp Expr
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

-- the actual parser
identifier :: Parser Ident
identifier = lexeme $ (:) <$> C.letterChar <*> many C.alphaNumChar

-- expression parser
tuple :: Parser Expr
tuple = Tuple <$> parens (expr `sepEndBy` symbol ",")

lambda :: Parser Expr
lambda = do
  symbol "\\"
  param <- some identifier
  symbol "=>"
  body <- expr
  return $ Lambda param body

operators :: [[Operator Parser Expr]]
operators =
  [ [ InfixL (Apply <$ symbol "") ],
    [ Prefix (SingleOp Op.Positive <$ symbol "+")
    , Prefix (SingleOp Op.Negative <$ symbol "-") ],
    [ InfixL (BinaryOp Op.Mul <$ symbol "*") ],
    [ InfixL (BinaryOp Op.Add <$ symbol "+") ] ]

term :: Parser Expr
term = try (parens expr)
  <|> tuple
  <|> lambda
  <|> Variable <$> identifier
  <|> Integer <$> integer

expr :: Parser Expr
expr = makeExprParser term operators

-- wrap them up
parser :: Parser Expr
parser = between space eof expr

newtype ParseError = ParseError String deriving (Show)

parse_expr :: String -> String -> Either ParseError Expr
parse_expr name input = left pretty $ parse parser name input
  where
    pretty = ParseError . errorBundlePretty
