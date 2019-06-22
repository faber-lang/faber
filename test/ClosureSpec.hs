module ClosureSpec (spec) where

import Test.Hspec

import           Closure
import qualified Lazy    as L

spec :: Spec
spec = do
  describe "closure conversion" $ do
    it "convertExpr lambdas" $ do
      -- \. 0
      convertExpr (L.Lambda $ L.ParamBound 0) `shouldBe` Tuple [Function Parameter, Tuple []]
      -- \.\. 0 1
      convertExpr (L.Lambda (
                L.Lambda (
                  L.Apply (L.ParamBound 0) (L.ParamBound 1)
                )
              )) `shouldBe` (
              Tuple [
                Function (
                  Tuple [
                    Function (
                      Apply Parameter (NthOf 0 Env)
                    ),
                    Tuple [
                      Parameter
                    ]
                  ]
                ),
                Tuple []
              ])

    it "convertExpr lambdas with multiple occured parameter" $ do
      -- \. 0 0
      convertExpr (L.Lambda $ L.Apply (L.ParamBound 0) (L.ParamBound 0)) `shouldBe` Tuple [Function (Apply Parameter Parameter), Tuple []]
      -- \.\. 0 1 0 1
      convertExpr (L.Lambda (
                L.Lambda (
                  L.Apply (
                    L.Apply (
                      L.Apply (L.ParamBound 0) (L.ParamBound 1)
                    )
                    (L.ParamBound 0)
                  )
                  (L.ParamBound 1)
                )
              )) `shouldBe` (
              Tuple [
                Function (
                  Tuple [
                    Function (
                      Apply (
                        Apply (
                          Apply Parameter (NthOf 0 Env)
                        )
                        Parameter
                      )
                      (NthOf 0 Env)
                    ),
                    Tuple [
                      Parameter
                    ]
                  ]
                ),
                Tuple []
              ])

    it "convertExpr deeply nested lambdas" $ do
      -- \.\.\. 0 1 2
      convertExpr (L.Lambda (
                L.Lambda (
                  L.Lambda (
                    L.Apply (
                      L.Apply (L.ParamBound 2) (L.ParamBound 1)
                    )
                    (L.ParamBound 0)
                  )
                )
              )) `shouldBe` (
              Tuple [
                Function (
                  Tuple [
                    Function (
                      Tuple [
                        Function (
                          Apply (
                            Apply (NthOf 0 Env) (NthOf 1 Env)
                          )
                          Parameter
                        ),
                        Tuple [
                          NthOf 0 Env,
                          Parameter
                        ]
                      ]
                    ),
                    Tuple [Parameter]
                  ]
                ),
                Tuple []
              ])
