module LazySpec (spec) where

import           Lazy
import qualified Nameless   as N
import qualified Operators  as Op
import           Test.Hspec

spec :: Spec
spec = do
  describe "lazy to strict conversion" $ do
    it "converts simple expressions" $ do
      lazy (N.Apply (N.Integer 1) (N.BinaryOp Op.Add (N.Integer 1) (N.Integer 1))) `shouldBe` Apply (Integer 1) (Ref (Tuple [Integer 0, Lambda $ NthOf 1 (Assign (Bound 0) (Tuple [Integer 1, BinaryOp Op.Add (Integer 1) (Integer 1)]))]))
      lazy (N.Bound 0) `shouldBe` LocalLet (Deref (Bound 0)) (If (NthOf 0 LetBound) (NthOf 1 LetBound) (Apply (NthOf 1 LetBound) (Bound 0)))
