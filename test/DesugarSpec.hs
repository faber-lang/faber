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
      D.desugar [P.Name (P.NameDef "f" ["a", "b", "c"] (P.Integer 1) [])] `shouldBe` [D.Name $ D.NameDef "f" $ D.Lambda "a" (D.Lambda "b" (D.Lambda "c" (D.Integer 1)))]

    it "convert where clause" $ do
      D.desugar [P.Name (P.NameDef "f" [] (P.Variable "x") [P.NameDef "x" [] (P.Integer 1) []])] `shouldBe` [D.Name $ D.NameDef "f" $ D.LetIn [D.NameDef "x" $ D.Integer 1] $ D.Variable "x"]
