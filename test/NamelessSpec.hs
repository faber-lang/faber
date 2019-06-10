module NamelessSpec (spec) where

import qualified Desugar    as D
import qualified Nameless   as N
import           Test.Hspec

spec :: Spec
spec = do
  describe "basic conversion" $ do
    it "convert variable indices" $ do
      N.nameless (D.Lambda "x" (D.Variable "x")) `shouldBe` N.Lambda (N.Bound 0)
      N.nameless (D.Lambda "x" (D.Lambda "y" (D.Variable "x"))) `shouldBe` N.Lambda (N.Lambda (N.Bound 1))

    it "convert shadowing names" $ do
      N.nameless (D.Lambda "x" (D.Lambda "x" (D.Variable "x"))) `shouldBe` N.Lambda (N.Lambda (N.Bound 0))

  describe "complex examples" $ do
    it "complex example 1" $ do
      N.nameless (D.Lambda "z" (
                    D.Apply (
                      D.Lambda "y" (
                        D.Apply (D.Variable "y") (D.Lambda "x" (D.Variable "x"))
                      )
                    )
                    (
                      D.Lambda "x" (
                        D.Apply (D.Variable "z") (D.Variable "x")
                      )
                    )
                  )) `shouldBe` (
                  N.Lambda (
                    N.Apply (
                      N.Lambda (
                        N.Apply (N.Bound 0) (N.Lambda (N.Bound 0))
                      )
                    )
                    (
                      N.Lambda (
                        N.Apply (N.Bound 1) (N.Bound 0)
                      )
                    )
                  ))

    it "complex example 2, z combinator" $ do
      N.nameless (D.Lambda "f" (
                    D.Apply (
                      D.Lambda "x" (
                        D.Apply
                          (D.Variable "f")
                          (D.Lambda "y" (
                            D.Apply (D.Apply (D.Variable "x") (D.Variable "x")) (D.Variable "y")
                          ))
                      )
                    )
                    (
                      D.Lambda "x" (
                        D.Apply
                          (D.Variable "f")
                          (D.Lambda "y" (
                            D.Apply (D.Apply (D.Variable "x") (D.Variable "x")) (D.Variable "y")
                          ))
                      )
                    )
                  )) `shouldBe` (
                  N.Lambda (
                    N.Apply (
                      N.Lambda (
                        N.Apply
                          (N.Bound 1)
                          (N.Lambda (
                            N.Apply (N.Apply (N.Bound 1) (N.Bound 1)) (N.Bound 0)
                          ))
                      )
                    )
                    (
                      N.Lambda (
                        N.Apply
                          (N.Bound 1)
                          (N.Lambda (
                            N.Apply (N.Apply (N.Bound 1) (N.Bound 1)) (N.Bound 0)
                          ))
                      )
                    )
                  ))
