module ClosureSpec (spec) where

import Test.Hspec

import           Closure
import qualified Lazy    as L

spec :: Spec
spec = do
  describe "convert conversion" $ do
    it "convert lambdas" $ do
      -- \. 0
      closureExpr (L.Lambda $ L.ParamBound 0) `shouldBe` Tuple [Function Parameter, Tuple []]
      -- \.\. 0 1
      closureExpr (L.Lambda (
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

    it "convert lambdas with reference to the global name" $ do
      closureExpr (L.Apply (L.GlobalBound "f") (L.Lambda $ L.GlobalBound "g")) `shouldBe` Apply (GlobalName "f") (Tuple [Function $ NthOf 0 Env, Tuple [GlobalName "g"]])

    it "convert lambdas with multiple occured parameter" $ do
      -- \. 0 0
      closureExpr (L.Lambda $ L.Apply (L.ParamBound 0) (L.ParamBound 0)) `shouldBe` Tuple [Function (Apply Parameter Parameter), Tuple []]
      -- \.\. 0 1 0 1
      closureExpr (L.Lambda (
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

    it "convert deeply nested lambdas" $ do
      -- \.\.\. 0 1 2
      closureExpr (L.Lambda (
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
