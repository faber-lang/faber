module Parse where

import           Control.Arrow
import           Control.Monad.Combinators.Expr
import           Data.Functor                   (void)
import           Data.Maybe                     (fromMaybe)
import           Data.Void
import           Text.Megaparsec                hiding (ParseError)
import qualified Text.Megaparsec.Char           as C
import qualified Text.Megaparsec.Char.Lexer     as L
import           Text.Megaparsec.Error          (errorBundlePretty)

import qualified Operators as Op

-- syntax tree
type Ident = String

data Pattern
  = PVar Ident
  | PWildcard
  | PInt Int
  | PCtor String [Pattern]
  | PTuple [Pattern]
  deriving (Show, Eq)

data Expr
  = Integer Int
  | Lambda [Ident] Expr
  | Apply Expr Expr
  | Variable Ident
  | Tuple [Expr]
  | BinaryOp Op.BinaryOp Expr Expr
  | SingleOp Op.SingleOp Expr
  | LetIn [NameDef] Expr
  | If Expr Expr Expr
  | Match Expr [(Pattern, Expr)]
  deriving (Show, Eq)

data TypeExpr
  = Ident String
  | Function TypeExpr TypeExpr
  | Product [TypeExpr]
  | ApplyTy TypeExpr TypeExpr
  deriving (Show, Eq)

-- a : Num -> TypeConstraint "a" "Num"
data TypeConstraint
  = TypeConstraint String String
  deriving (Show, Eq)

data TypeScheme
  = Forall [String] [TypeConstraint] TypeExpr
  deriving (Show, Eq)

data NameDef
  = NameDef String [Ident] Expr [NameDef]
  | TypeAnnot String TypeScheme
  deriving (Show, Eq)

newtype TypeDef
  = Variant [(String, [TypeExpr])]
  deriving (Show, Eq)

-- class Num with a of a : Eq where <defs> -> ClassDef "Num" "a" [TypeConstraint "a" "Eq"] defs
data ClassDef
  = ClassDef String String [TypeConstraint] [NameDef]
  deriving (Show, Eq)

-- impl Num for forall a. a where <defs> -> InstDef "Num" (Forall ["a"] $ Ident "a") defs
data InstDef
  = InstDef String TypeScheme [NameDef]
  deriving (Show, Eq)

data Def
  = Name NameDef
  | Type String [String] TypeDef
  | Class ClassDef
  | Instance InstDef
  deriving (Show, Eq)

type Code = [Def]

-- parser type definition
type Parser = Parsec Void String

-- lexer utils
headRws :: [Parser ()]
headRws = [char_ '-', string_ "name", string_ "type", string_ "class", string_ "impl"]
  where
    char_   = void . C.char
    string_ = void . C.string

space :: Parser ()
space = L.space skip line block
  where
    line = L.skipLineComment "//"
    block = L.skipBlockComment "/*" "*/"
    skip = notFollowedBy (lexeme_ C.newline >> choice headRws) >> (lexeme_ C.newline <|> sc)
    sc = void $ some (C.char ' ' <|> C.char '\t')

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

lexeme_ :: Parser a -> Parser ()
lexeme_ = void . lexeme

symbol :: String -> Parser ()
symbol = void . L.symbol space

newline :: Parser ()
newline = lexeme_ C.newline

integer :: Parser Int
integer = lexeme L.decimal

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

rword :: String -> Parser ()
rword w = (lexeme . try) (C.string w *> notFollowedBy C.alphaNumChar)

-- the actual parser
identifier' :: [String] -> Parser Ident
identifier' rws = (lexeme . try) (p >>= check)
  where
    p = (:) <$> C.letterChar <*> many C.alphaNumChar
    check x | x `elem` rws = fail $ "attempt to parse " ++ show x ++ "as an identifier"
            | otherwise    = return x

-- pattern parser
patIdentifier :: Parser Ident
patIdentifier = identifier' []

patWildcard :: Parser Pattern
patWildcard = symbol "_" >> return PWildcard

patTuple :: Parser Pattern
patTuple = PTuple <$> parens (pattern_ `sepEndBy` symbol ",")

patCtor :: Parser Pattern
patCtor = PCtor <$> (symbol "#" >> identifier) <*> many pattern_

pattern_ :: Parser Pattern
pattern_ = try (parens pattern_)
  <|> patTuple
  <|> patWildcard
  <|> patCtor
  <|> PVar <$> patIdentifier
  <|> PInt <$> integer

-- expression parser
exprRws :: [String]
exprRws = ["let", "in", "where", "if", "then", "else", "match", "with"]

identifier :: Parser Ident
identifier = identifier' exprRws

tuple :: Parser Expr
tuple = Tuple <$> parens (expr `sepEndBy` symbol ",")

lambda :: Parser Expr
lambda = do
  symbol "\\"
  param <- some identifier
  symbol "=>"
  Lambda param <$> expr

letIn :: Parser Expr
letIn = do
  rword "let"
  defs <- nameDefs
  rword "in"
  LetIn defs <$> expr

ifThenElse :: Parser Expr
ifThenElse = do
  rword "if"
  cond <- expr
  rword "then"
  then_ <- expr
  rword "else"
  If cond then_ <$> expr

match_ :: Parser Expr
match_ = do
  rword "match"
  target <- expr
  rword "with"
  _ <- optional $ symbol "|"
  Match target <$> arm `sepBy1` symbol "|"
  where
    arm = do
      pat <- pattern_
      symbol "->"
      body <- expr
      return (pat, body)

operators :: [[Operator Parser Expr]]
operators =
  [ [ InfixL (Apply <$ symbol "") ],
    [ Prefix (SingleOp Op.Positive <$ symbol "+")
    , Prefix (SingleOp Op.Negative <$ symbol "-") ],
    [ InfixL (BinaryOp Op.Mul <$ symbol "*") ],
    [ InfixL (BinaryOp Op.Add <$ symbol "+")
    , InfixL (BinaryOp Op.Sub <$ symbol "-") ],
    [ InfixL (BinaryOp Op.Eq <$ symbol "==") ] ]

term :: Parser Expr
term = try (parens expr)
  <|> tuple
  <|> letIn
  <|> ifThenElse
  <|> match_
  <|> lambda
  <|> Variable <$> identifier
  <|> Integer <$> integer

expr :: Parser Expr
expr = makeExprParser term operators

-- type expression parser
typeRws :: [String]
typeRws = ["where"]

typeIdentifier :: Parser String
typeIdentifier = identifier' typeRws

typeOperators :: [[Operator Parser TypeExpr]]
typeOperators =
  [ InfixL (ApplyTy <$ symbol "") ] : typeOperatorsNoApp

typeOperatorsNoApp :: [[Operator Parser TypeExpr]]
typeOperatorsNoApp =
  [ [ InfixR (Function <$ symbol "->") ] ]

typeProd :: Parser TypeExpr
typeProd = Product <$> parens (typeExpr `sepEndBy` symbol ",")

typeTerm :: Parser TypeExpr
typeTerm = try (parens typeExpr)
  <|> typeProd
  <|> Ident <$> typeIdentifier

typeExpr :: Parser TypeExpr
typeExpr = makeExprParser typeTerm typeOperators

typeExprNoApp :: Parser TypeExpr
typeExprNoApp = makeExprParser typeTerm typeOperatorsNoApp

-- type constraints parser
typeConstraint :: Parser TypeConstraint
typeConstraint = TypeConstraint <$> (typeIdentifier <* symbol ":") <*> typeIdentifier

typeConstraints :: Parser [TypeConstraint]
typeConstraints = typeConstraint `sepBy1` symbol ","

-- type scheme parser
typeScheme :: Parser TypeScheme
typeScheme = Forall <$> binder <*> cstrs <*> typeExpr
  where
    binder = fromMaybe [] <$> optional forallBinder
    cstrs = fromMaybe [] <$> optional (try typeConstraints <* symbol "=>")
    forallBinder = do
      symbol "forall"
      vars <- some typeIdentifier
      symbol "."
      return vars

-- definition parser
nameValueDef :: Parser NameDef
nameValueDef = do
  name <- identifier
  params <- many identifier
  symbol "="
  body <- expr
  defs <- fromMaybe [] <$> optional where_
  return $ NameDef name params body defs
  where
    where_ = rword "where" >> nameDefs

nameAnnotDef :: Parser NameDef
nameAnnotDef = do
  name <- identifier
  symbol "::"
  TypeAnnot name <$> typeScheme

nameDef :: Parser NameDef
nameDef = try nameAnnotDef <|> nameValueDef

nameDefs :: Parser [NameDef]
nameDefs = many (optional hyphen >> nameDef)
  where
    hyphen = try (newline >> symbol "-")

newlineDelim :: String -> Parser ()
newlineDelim s = try (optional newline >> symbol s)

defName :: Parser Def
defName = Name <$> (newlineDelim "name" >> nameDef)

defType :: Parser Def
defType = newlineDelim "type" >> body
  where
    body = do
      name <- identifier
      vars <- many identifier
      symbol "="
      _ <- optional $ symbol "|"
      Type name vars . Variant <$> variant `sepBy1` symbol "|"
    variant = do
      ctor <- identifier
      -- Cons a (List a) should be parsed as [a, ApplyTy List a], not [ApplyTy a (ApplyTy List a)]
      params <- many typeExprNoApp
      return (ctor, params)

classDef :: Parser ClassDef
classDef = do
  name <- typeIdentifier
  symbol "with"
  tv <- typeIdentifier
  cstrs <- fromMaybe [] <$> optional (symbol "of" >> typeConstraints)
  symbol "where"
  ClassDef name tv cstrs <$> nameDefs

instDef :: Parser InstDef
instDef = do
  name <- typeIdentifier
  symbol "for"
  scheme <- typeScheme
  symbol "where"
  InstDef name scheme <$> nameDefs

defClass :: Parser Def
defClass = Class <$> (newlineDelim "class" >> classDef)

defInst :: Parser Def
defInst = Instance <$> (newlineDelim "impl" >> instDef)

definition :: Parser Def
definition = defName <|> defType <|> defClass <|> defInst

definitions :: Parser [Def]
definitions = some definition

-- wrap them up
code :: Parser Code
code = definitions

parser :: Parser Code
parser = between space eof code

newtype ParseError = ParseError String deriving (Show, Eq)

parseCode :: String -> String -> Either ParseError Code
parseCode name input = left pretty $ parse parser name input
  where
    pretty = ParseError . errorBundlePretty
