module ParseSpec (spec) where

import Operators
import Parse
import Test.Hspec

-- helpers
parse :: String -> Expr
parse s = case parseExpr "" s of
  Left (ParseError err) -> error err
  Right t               -> t

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