module NamelessSpec (spec) where


import qualified Desugar  as D
import qualified Nameless as N

import           Control.Monad.Reader
import qualified Data.Map             as Map
import           Test.Hspec

namelessExpr :: D.Expr -> N.Expr
namelessExpr e = runReader (N.namelessExpr e) N.initEnv

spec :: Spec
spec = do
  describe "basic conversion" $ do
    it "convert variable indices" $ do
      namelessExpr (D.Lambda "x" (D.Variable "x")) `shouldBe` N.Lambda (N.ParamBound 0)
      namelessExpr (D.Lambda "x" (D.Lambda "y" (D.Variable "x"))) `shouldBe` N.Lambda (N.Lambda (N.ParamBound 1))

    it "convert shadowing names" $ do
      namelessExpr (D.Lambda "x" (D.Lambda "x" (D.Variable "x"))) `shouldBe` N.Lambda (N.Lambda (N.ParamBound 0))

  describe "global names" $ do
    it "convert global name reference" $ do
      N.nameless [D.Name (D.NameDef "a" $ D.Integer 1), D.Name (D.NameDef "main" $ D.Variable "a")] `shouldBe` N.Code Map.empty [N.Name "a" $ N.Integer 1, N.Name "main" $ N.GlobalBound "a" 0]

  describe "complex examples" $ do
    it "complex example 1" $ do
      namelessExpr (D.Lambda "z" (
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
                        N.Apply (N.ParamBound 0) (N.Lambda (N.ParamBound 0))
                      )
                    )
                    (
                      N.Lambda (
                        N.Apply (N.ParamBound 1) (N.ParamBound 0)
                      )
                    )
                  ))

    it "complex example 2, z combinator" $ do
      namelessExpr (D.Lambda "f" (
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
                          (N.ParamBound 1)
                          (N.Lambda (
                            N.Apply (N.Apply (N.ParamBound 1) (N.ParamBound 1)) (N.ParamBound 0)
                          ))
                      )
                    )
                    (
                      N.Lambda (
                        N.Apply
                          (N.ParamBound 1)
                          (N.Lambda (
                            N.Apply (N.Apply (N.ParamBound 1) (N.ParamBound 1)) (N.ParamBound 0)
                          ))
                      )
                    )
                  ))
