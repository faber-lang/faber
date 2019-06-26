module HoistSpec (spec) where

import qualified Closure    as C
import           Hoist
import           Test.Hspec

hoistTest :: C.Expr -> ([Function], Expr)
hoistTest = destruct . hoist . makeCode
  where
    -- this integer is dummy
    makeCode e = C.Code [C.Name "main" e] (C.Integer 0)
    destruct (Module funs (Code [Name "main" e] _)) = (funs, e)

spec :: Spec
spec = do
  describe "function hoisting" $ do
    it "hoist functions" $ do
      hoistTest (C.Function C.Parameter) `shouldBe` ([Function 2 (Parameter 1)], FunctionRef 0)
      hoistTest (C.Function $ C.Function $ C.Parameter) `shouldBe` ([Function 2 (Parameter 1), Function 2 (FunctionRef 0)], FunctionRef 1)

    it "convert function calls" $ do
      hoistTest (C.Apply (
              C.Tuple [
                C.Function C.Parameter,
                C.Tuple []
              ])
              (C.Integer 1)
            ) `shouldBe` ([Function 2 $ Parameter 1] ,
              LocalLet (Tuple [FunctionRef 0, Tuple []])
              (
                Call (NthOf 0 LocalBound) [NthOf 1 LocalBound, Integer 1]
              )
            )
