module DesugarSpec (spec) where

import qualified Desugar    as D
import qualified Parse      as P
import           Test.Hspec

spec :: Spec
spec = do
  describe "desugar" $ do
    it "convert lambdas" $ do
      D.desugarExpr (P.Lambda ["a", "b", "c"] (P.Integer 1)) `shouldBe` D.Lambda "a" (D.Lambda "b" (D.Lambda "c" (D.Integer 1)))

    it "convert parameters of definitions" $ do
      D.desugar [P.Def "f" (P.Name ["a", "b", "c"] (P.Integer 1) [])] `shouldBe` [D.Def "f" $ D.Name $ D.Lambda "a" (D.Lambda "b" (D.Lambda "c" (D.Integer 1)))]

    it "convert where clause" $ do
      D.desugar [P.Def "f" (P.Name [] (P.Variable "x") [P.Def "x" (P.Name [] (P.Integer 1) [])])] `shouldBe` [D.Def "f" $ D.Name $ D.LetIn [D.Def "x" $ D.Name $ D.Integer 1] $ D.Variable "x"]
