module TypingSpec (spec) where

import Test.Hspec

import           Control.Arrow (right)
import           Nameless
import           Operators
import qualified Typing        as T

-- helpers
typeExpr :: Expr -> Either T.TypeError T.Type
typeExpr = (right snd) . T.runInfer . T.inferExpr

expectError :: Expr -> String
expectError e = case typeExpr e of
  Left err -> show err
  Right t  -> error $ "typing error is expected, but typed: " ++ show t

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

var :: Int -> Expr
var = ParamBound

-- test cases
spec :: Spec
spec = do
  describe "types" $ do
    it "type simple terms" $ do
      typeExpr (int 10) `shouldBe` Right T.Integer
      typeExpr (int 1 `add` int 2) `shouldBe` Right T.Integer
      typeExpr (neg $ int 3) `shouldBe` Right T.Integer

    it "fail in obviously invalid situation" $ do
      expectError (Apply (int 1) (int 2)) `shouldContain` "UnificationFail"
      expectError (Lambda (var 0) `add` int 1) `shouldContain` "UnificationFail"

    it "type terms with lambdas" $ do
      typeExpr (Lambda $ var 0 `add` var 0) `shouldBe` Right (T.Function T.Integer T.Integer)
      typeExpr (Apply (Lambda $ var 0) (int 1)) `shouldBe` Right T.Integer
      typeExpr (Apply (Apply (Lambda $ Lambda $ Apply (var 0) (var 1)) (int 10)) (Lambda $ var 0)) `shouldBe` Right T.Integer

    it "type terms with complex unification" $ do
      typeExpr (Apply (Apply (Apply (Lambda $ Lambda $ Apply (var 0) (var 1)) (int 10)) (Lambda $ Lambda $ var 0 `add` var 1)) (int 1)) `shouldBe` Right T.Integer
