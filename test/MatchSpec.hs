module MatchSpec (spec) where

import qualified Desugar    as D
import qualified Errors     as Err
import           Match
import qualified Operators  as Op
import           Parse      (Pattern (..))
import           Test.Hspec

spec :: Spec
spec = do
  describe "compile pattern matching" $ do
    it "convert integer pattern" $ do
      convertExpr (D.Match (D.Integer 1) [(PInt 1, D.Integer 2)]) `shouldBe` (LetIn [NameDef "_match0" (Integer 1)] (If (BinaryOp Op.Eq (Variable "_match0") (Integer 1)) (Integer 2) (Error Err.MatchFail)))
    it "convert variable pattern" $ do
      convertExpr (D.Match (D.Integer 1) [(PVar "a", D.Integer 1)]) `shouldBe` (LetIn [NameDef "_match0" (Integer 1)] (LetIn [NameDef "a" (Variable "_match0")] (Integer 1)))
    -- some integration tests are provided to made up for missing test cases
