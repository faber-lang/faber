module Parse where

import           Control.Arrow
import           Control.Monad.Combinators.Expr
import           Data.Void
import qualified Operators                      as Op
import           Text.Megaparsec                hiding (ParseError)
import qualified Text.Megaparsec.Char           as C
import qualified Text.Megaparsec.Char.Lexer     as L
import           Text.Megaparsec.Error          (errorBundlePretty)

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
  deriving (Show, Eq)

data DefBody
  = Name [Ident] Expr
  deriving (Show, Eq)

data Def = Def String DefBody deriving (Show, Eq)

type Code = [Def]

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
  Lambda param <$> expr

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

-- definition parser
nameDef :: Parser Def
nameDef = do
  name <- identifier
  params <- many identifier
  symbol "="
  body <- expr
  symbol ";;"
  return $ Def name $ Name params body

definition :: Parser Def
definition = nameDef

-- wrap them up
code :: Parser Code
code = many definition

parser :: Parser Code
parser = between space eof code

newtype ParseError = ParseError String deriving (Show, Eq)

parseCode :: String -> String -> Either ParseError Code
parseCode name input = left pretty $ parse parser name input
  where
    pretty = ParseError . errorBundlePretty
