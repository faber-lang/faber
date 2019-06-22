module DesugarSpec (spec) where

import qualified Desugar    as D
import qualified Parse      as P
import           Test.Hspec

spec :: Spec
spec = do
  describe "desugar" $ do
    it "convert lambdas" $ do
      D.desugarExpr (P.Lambda ["a", "b", "c"] (P.Integer 1)) `shouldBe` D.Lambda "a" (D.Lambda "b" (D.Lambda "c" (D.Integer 1)))
