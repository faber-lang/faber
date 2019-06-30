module TypingSpec (spec) where

import Control.Arrow (right)
import Test.Hspec

import           Nameless
import           Operators
import qualified Parse     as P
import qualified Typing    as T
import           Utils

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

fRef :: Expr
fRef = LetBound $ LetIndex 0 0 0

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

  describe "polymorphism" $ do
    it "type poymorphic functions" $ do
      -- let f x = x in f f (f 1)
      typeExpr (LetIn [Nothing] [Lambda $ var 0] $ Apply (Apply fRef fRef) (Apply fRef (int 1))) `shouldBe` Right T.Integer

    it "doesn't generalize lambda params" $ do
      -- (\f => (f 0, f (\x => x))) (\x => x)
      expectError (Apply (Lambda $ Tuple [Apply (var 0) (int 0), Apply (var 0) (Lambda $ var 0)]) (Lambda $ var 0)) `shouldContain` "UnificationFail"

  describe "type annotation on name definitions" $ do
    it "unify with inferred type" $ do
      expectError (LetIn [Just $ P.Forall [] $ P.Function (P.Product []) (P.Product [])] [Lambda $ int 0] $ int 0) `shouldContain` "UnificationFail"
      expectError (LetIn [Just $ P.Forall [] $ P.Function (P.Product []) (P.Product [])] [Lambda $ var 0] $ Apply fRef (int 0)) `shouldContain` "UnificationFail"

    it "won't unify with rigid type variables" $ do
      expectError (LetIn [Just $ P.Forall ["a"] $ P.Function (P.Ident "a") (P.Ident "a")] [Lambda $ int 0] $ int 0) `shouldContain` "RigidUnificationFail"
