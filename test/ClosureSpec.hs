module ClosureSpec (spec) where

import Test.Hspec

import           Closure
import qualified Flatten as F

spec :: Spec
spec = do
  describe "convert conversion" $ do
    it "convert lambdas" $ do
      -- \. 0
      closureExpr (F.Lambda $ F.ParamBound 0) `shouldBe` Tuple [Function Parameter, Tuple []]
      -- \.\. 0 1
      closureExpr (F.Lambda (
                F.Lambda (
                  F.Apply (F.ParamBound 0) (F.ParamBound 1)
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
      closureExpr (F.Apply (F.GlobalBound "f") (F.Lambda $ F.GlobalBound "g")) `shouldBe` Apply (GlobalName "f") (Tuple [Function $ NthOf 0 Env, Tuple [GlobalName "g"]])

    it "convert lambdas with multiple occured parameter" $ do
      -- \. 0 0
      closureExpr (F.Lambda $ F.Apply (F.ParamBound 0) (F.ParamBound 0)) `shouldBe` Tuple [Function (Apply Parameter Parameter), Tuple []]
      -- \.\. 0 1 0 1
      closureExpr (F.Lambda (
                F.Lambda (
                  F.Apply (
                    F.Apply (
                      F.Apply (F.ParamBound 0) (F.ParamBound 1)
                    )
                    (F.ParamBound 0)
                  )
                  (F.ParamBound 1)
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
      closureExpr (F.Lambda (
                F.Lambda (
                  F.Lambda (
                    F.Apply (
                      F.Apply (F.ParamBound 2) (F.ParamBound 1)
                    )
                    (F.ParamBound 0)
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
