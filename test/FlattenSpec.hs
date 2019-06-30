module FlattenSpec (spec) where

import           Flatten
import qualified Lazy       as L
import qualified Nameless   as N
import qualified Operators  as Op
import           Test.Hspec

boundL :: Int -> Int -> L.Expr
boundL loc inn = L.LetBound $ N.LetIndex 0 loc inn

boundF :: Int -> Expr
boundF loc = LetBound $ LetIndex 0 loc

addL :: L.Expr -> L.Expr -> L.Expr
addL = L.BinaryOp Op.Add

addF :: Expr -> Expr -> Expr
addF = BinaryOp Op.Add

spec :: Spec
spec = do
  describe "flatten of recursive definitions" $ do
    it "simple recursion" $ do
      flattenExpr (L.LetIn [boundL 0 0] (boundL 0 0)) `shouldBe` LetIn Alloc (Seq (Assign (boundF 0) (Deref $ boundF 0)) (Deref (boundF 0)))
    it "mutual recursion" $ do
      flattenExpr (L.LetIn [boundL 0 1, boundL 0 0] (boundL 0 0 `addL` boundL 0 1)) `shouldBe` LetIn Alloc (LetIn Alloc $ Seq (Assign (boundF 0) (Deref $ boundF 1)) (Seq (Assign (boundF 1) (Deref $ boundF 0)) (Deref (boundF 0) `addF` Deref (boundF 1))))
    it "nested let" $ do
      flattenExpr (L.LetIn [L.Integer 1] $ L.LetIn [boundL 1 0] (boundL 1 0 `addL` boundL 0 0)) `shouldBe` LetIn Alloc (Seq (Assign (boundF 0) (Integer 1)) (LetIn Alloc (Seq (Assign (boundF 0) (Deref $ boundF 1)) (Deref (boundF 1) `addF` Deref (boundF 0)))))
