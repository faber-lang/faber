module ClosureSpec (spec) where

import Test.Hspec

import           Closure
import qualified Lazy    as L

spec :: Spec
spec = do
  describe "closure conversion" $ do
    it "convert lambdas" $ do
      -- \. 0
      convert (L.Lambda $ L.Bound 0) `shouldBe` Tuple [Function Parameter, Tuple []]
      -- \.\. 0 1
      convert (L.Lambda (
                L.Lambda (
                  L.Apply (L.Bound 0) (L.Bound 1)
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

    it "convert lambdas with multiple occured parameter" $ do
      -- \. 0 0
      convert (L.Lambda $ L.Apply (L.Bound 0) (L.Bound 0)) `shouldBe` Tuple [Function (Apply Parameter Parameter), Tuple []]
      -- \.\. 0 1 0 1
      convert (L.Lambda (
                L.Lambda (
                  L.Apply (
                    L.Apply (
                      L.Apply (L.Bound 0) (L.Bound 1)
                    )
                    (L.Bound 0)
                  )
                  (L.Bound 1)
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
      convert (L.Lambda (
                L.Lambda (
                  L.Lambda (
                    L.Apply (
                      L.Apply (L.Bound 2) (L.Bound 1)
                    )
                    (L.Bound 0)
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
