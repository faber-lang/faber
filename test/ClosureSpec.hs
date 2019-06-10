module ClosureSpec (spec) where

import Test.Hspec

import qualified Nameless as N
import Closure

spec :: Spec
spec = do
  describe "closure conversion" $ do
    it "convert lambdas" $ do
      -- \. 0
      convert (N.Lambda $ N.Bound 0) `shouldBe` Tuple [Function Parameter, Tuple []]
      -- \.\. 0 1
      convert (N.Lambda (
                N.Lambda (
                  N.Apply (N.Bound 0) (N.Bound 1)
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
      convert (N.Lambda $ N.Apply (N.Bound 0) (N.Bound 0)) `shouldBe` Tuple [Function (Apply Parameter Parameter), Tuple []]
      -- \.\. 0 1 0 1
      convert (N.Lambda (
                N.Lambda (
                  N.Apply (
                    N.Apply (
                      N.Apply (N.Bound 0) (N.Bound 1)
                    )
                    (N.Bound 0)
                  )
                  (N.Bound 1)
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
      convert (N.Lambda (
                N.Lambda (
                  N.Lambda (
                    N.Apply (
                      N.Apply (N.Bound 2) (N.Bound 1)
                    )
                    (N.Bound 0)
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
                            Apply (NthOf 1 Env) (NthOf 0 Env)
                          )
                          Parameter
                        ),
                        Tuple [
                          Parameter,
                          NthOf 0 Env
                        ]
                      ]
                    ),
                    Tuple [Parameter]
                  ]
                ),
                Tuple []
              ])
