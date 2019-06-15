module HoistSpec (spec) where

import qualified Closure    as C
import           Hoist
import           Test.Hspec

spec :: Spec
spec = do
  describe "function hoisting" $ do
    it "hoist functions" $ do
      hoist (C.Function C.Parameter) `shouldBe` Module [Function 2 (Parameter 1)] (FunctionRef 0)
      hoist (C.Function $ C.Function $ C.Parameter) `shouldBe` Module [Function 2 (Parameter 1), Function 2 (FunctionRef 0)] (FunctionRef 1)

    it "convert function calls" $ do
      hoist (C.Apply (
              C.Tuple [
                C.Function C.Parameter,
                C.Tuple []
              ])
              (C.Integer 1)
            ) `shouldBe` Module [Function 2 $ Parameter 1] (
              LocalLet (Tuple [FunctionRef 0, Tuple []])
              (
                Call (NthOf 0 LetBound) [NthOf 1 LetBound, Integer 1]
              )
            )
