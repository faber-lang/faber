module TypingSpec (spec) where

import Test.Hspec

import           Nameless
import           Operators
import qualified Typing    as T

-- helpers
expect_error :: Expr -> String
expect_error e = case T.typing e of
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
var = Bound

-- test cases
spec :: Spec
spec = do
  describe "types" $ do
    it "type simple terms" $ do
      T.typing (int 10) `shouldBe` Right T.Integer
      T.typing (int 1 `add` int 2) `shouldBe` Right T.Integer
      T.typing (neg $ int 3) `shouldBe` Right T.Integer

    it "fail in obviously invalid situation" $ do
      expect_error (Apply (int 1) (int 2)) `shouldContain` "UnificationFail"
      expect_error (Lambda (var 0) `add` int 1) `shouldContain` "UnificationFail"

    it "type terms with lambdas" $ do
      T.typing (Lambda $ var 0 `add` var 0) `shouldBe` Right (T.Function T.Integer T.Integer)
      T.typing (Apply (Lambda $ var 0) (int 1)) `shouldBe` Right T.Integer
      T.typing (Apply (Apply (Lambda $ Lambda $ Apply (var 0) (var 1)) (int 10)) (Lambda $ var 0)) `shouldBe` Right T.Integer

    it "type terms with complex unification" $ do
      T.typing (Apply (Apply (Apply (Lambda $ Lambda $ Apply (var 0) (var 1)) (int 10)) (Lambda $ Lambda $ var 0 `add` var 1)) (int 1)) `shouldBe` Right T.Integer
