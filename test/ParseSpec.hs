module ParseSpec (spec) where

import Operators
import Parse
import Test.Hspec

-- helpers
parse :: String -> Expr
parse s = case parseCode "" code of
  Left (ParseError err) -> error err
  Right t               -> destruct t
  where
    code = "name main = " ++ s
    destruct [Name (NameDef _ [] body [])] = body

parseTy :: String -> TypeScheme
parseTy s = case parseCode "" code of
  Left (ParseError err) -> error err
  Right t               -> destruct t
  where
    code = "name main :: " ++ s
    destruct [Name (TypeAnnot _ body)] = body

add :: Expr -> Expr -> Expr
add = BinaryOp Add

mul :: Expr -> Expr -> Expr
mul = BinaryOp Mul

pos :: Expr -> Expr
pos = SingleOp Positive

neg :: Expr -> Expr
neg = SingleOp Negative

int :: Int -> Expr
int = Integer

var :: String -> Expr
var = Variable

-- tests
spec :: Spec
spec = do
  describe "skip" $ do
    it "skip spaces" $ do
      parse "24 " `shouldBe` int 24
      parse " 2 + 5 " `shouldBe` int 2 `add` int 5
      parse "- 4" `shouldBe` neg (int 4)
      parse " ( 24 + 12 )" `shouldBe` int 24 `add` int 12
      parse "\\a b c => 1+a*3 " `shouldBe` Lambda ["a", "b", "c"] (int 1 `add` (var "a" `mul` int 3))
      parse "( 1 ,2 ,3 ) " `shouldBe` Tuple [int 1, int 2, int 3]

    it "skip comments" $ do
      parse "24 /* comment */ + 23" `shouldBe` int 24 `add` int 23
      parse "1+ /* comment */ (/*comment*/2+3)" `shouldBe` int 1 `add` (int 2 `add` int 3)
      parse "\\abc=>\n//comment\n\\x//comment2\n=>/*comment*/x*abc" `shouldBe` Lambda ["abc"] (Lambda ["x"] (var "x" `mul` var "abc"))

  describe "expression" $ do
    it "parse integers" $ do
      parse "1" `shouldBe` int 1
      parse "24" `shouldBe` int 24
      parse "312" `shouldBe` int 312

    it "parse variables" $ do
      parse "a" `shouldBe` var "a"
      parse "abc" `shouldBe` var "abc"
      parse "x1" `shouldBe` var "x1"

    it "parse binary operators" $ do
      parse "2+5" `shouldBe` int 2 `add` int 5
      parse "12*35" `shouldBe` int 12 `mul` int 35

    it "binop precedence" $ do
      parse "2+5*10" `shouldBe` int 2 `add` (int 5 `mul` int 10)
      parse "1*2+3" `shouldBe` (int 1 `mul` int 2) `add` int 3
      parse "2+5*10+3" `shouldBe` (int 2 `add` (int 5 `mul` int 10)) `add` int 3

    it "binop associativity" $ do
      parse "1+2+3+4+5" `shouldBe` (((int 1 `add` int 2) `add` int 3) `add` int 4) `add` int 5
      parse "1*2*3*4*5" `shouldBe` (((int 1 `mul` int 2) `mul` int 3) `mul` int 4) `mul` int 5

    it "parse single operators" $ do
      parse "+5" `shouldBe` pos (int 5)
      parse "-4" `shouldBe` neg (int 4)
      parse "-x" `shouldBe` neg (var "x")

    it "parse parentheses" $ do
      parse "1+(2+3)" `shouldBe` int 1 `add` (int 2 `add` int 3)
      parse "3*(4+5)" `shouldBe` int 3 `mul` (int 4 `add` int 5)
      parse "(12)" `shouldBe` int 12
      parse "(24+12)" `shouldBe` int 24 `add` int 12
      parse "3*((4+5)*6)" `shouldBe` int 3 `mul` ((int 4 `add` int 5) `mul` int 6)

    it "parse lambdas" $ do
      parse "\\x=>x" `shouldBe` Lambda ["x"] (var "x")
      parse "\\a b c=>1+a*3" `shouldBe` Lambda ["a", "b", "c"] (int 1 `add` (var "a" `mul` int 3))
      parse "\\abc=>\\x=>x*abc" `shouldBe` Lambda ["abc"] (Lambda ["x"] (var "x" `mul` var "abc"))

    it "parse tuples" $ do
      parse "(1,2,3)" `shouldBe` Tuple [int 1, int 2, int 3]
      parse "(1+2,3+4)" `shouldBe` Tuple [int 1 `add` int 2, int 3 `add` int 4]
      parse "(0,)" `shouldBe` Tuple [int 0]
      parse "()" `shouldBe` Tuple []

    it "parse let-in" $ do
      parse "let a = 1 in a" `shouldBe` LetIn [NameDef "a" [] (int 1) []] (var "a")
      parse "let\n - a = 1\n - b = 2 in a" `shouldBe` LetIn [NameDef "a" [] (int 1) [], NameDef "b" [] (int 2) []] (var "a")

    it "parse nested let-in" $ do
      parse "let a = 1 in let b = 1 in a + b" `shouldBe` LetIn [NameDef "a" [] (int 1) []] (LetIn [NameDef "b" [] (int 1) []] (var "a" `add` var "b"))

    it "parse let-in with where clause" $ do
      parse "let a = b where b = 1 in a" `shouldBe` LetIn [NameDef "a" [] (var "b") [NameDef "b" [] (int 1) []]] (var "a")

    it "parse if-then-else" $ do
      parse "if 1 then a + 1 else b + 1" `shouldBe` If (int 1) (var "a" `add` int 1) (var "b" `add` int 1)
      parse "if 1 then if 0 then 1 else 2 else if 1 then 1 else 2" `shouldBe` If (int 1) (If (int 0) (int 1) (int 2)) (If (int 1) (int 1) (int 2))

  describe "type" $ do
    it "parse type identifiers" $ do
      parseTy "Int" `shouldBe` Forall [] (Ident "Int")
      parseTy "if" `shouldBe` Forall [] (Ident "if")

    it "parse function types" $ do
      parseTy "a -> b" `shouldBe` Forall [] (Function (Ident "a") (Ident "b"))
      parseTy "a -> a -> a" `shouldBe` Forall [] (Function (Ident "a") (Function (Ident "a") (Ident "a")))

    it "parse product types" $ do
      parseTy "(a, b)" `shouldBe` Forall [] (Product [Ident "a", Ident "b"])
      parseTy "(a,)" `shouldBe` Forall [] (Product [Ident "a"])
      parseTy "()" `shouldBe` Forall [] (Product [])

    it "parse parentheses" $ do
      parseTy "(a -> b) -> c" `shouldBe` Forall [] (Function (Function (Ident "a") (Ident "b")) (Ident "c"))
      parseTy "(a)" `shouldBe` Forall [] (Ident "a")

    it "parse quantifiers" $ do
      parseTy "forall a. a -> a" `shouldBe` Forall ["a"] (Function (Ident "a") (Ident "a"))
      parseTy "forall a b c. a" `shouldBe` Forall ["a", "b", "c"] (Ident "a")

  describe "definition" $ do
    it "parse simple name definitions" $ do
      parseCode "" "name def x y = x + y\nname main = 1" `shouldBe` Right [Name (NameDef "def" ["x", "y"] (add (var "x") (var "y")) []), Name (NameDef "main" [] (int 1) [])]

    it "parse name definitions with where" $ do
      parseCode "" "name def x = y where y = x" `shouldBe` Right [Name (NameDef "def" ["x"] (var "y") [NameDef "y" [] (var "x") []])]
      parseCode "" "name def x = y where\n - y = x\n  - z = x" `shouldBe` Right [Name (NameDef "def" ["x"] (var "y") [NameDef "y" [] (var "x") [], NameDef "z" [] (var "x") []])]
