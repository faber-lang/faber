module LazySpec (spec) where

import qualified Enum       as E
import           Lazy
import qualified Operators  as Op
import           Test.Hspec

spec :: Spec
spec = do
  describe "lazy to strict conversion" $ do
    it "converts simple expressions" $ do
      lazyExpr (E.Apply (E.Integer 1) (E.BinaryOp Op.Add (E.Integer 1) (E.Integer 1))) `shouldBe` Apply (Integer 1) (Ref (Tuple [Integer 0, Lambda $ NthOf 1 (Assign (ParamBound 0) (Tuple [Integer 1, BinaryOp Op.Add (Integer 1) (Integer 1)]))]))
      lazyExpr (E.ParamBound 0) `shouldBe` LocalLet (Deref (ParamBound 0)) (If (NthOf 0 LocalBound) (NthOf 1 LocalBound) (Apply (NthOf 1 LocalBound) (ParamBound 0)))
