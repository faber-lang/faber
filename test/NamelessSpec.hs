module NamelessSpec (spec) where


import qualified Match    as M
import qualified Nameless as N

import           Control.Monad.Reader
import qualified Data.Map             as Map
import           Test.Hspec

namelessExpr :: M.Expr -> N.Expr
namelessExpr e = runReader (N.namelessExpr e) N.initEnv

spec :: Spec
spec = do
  describe "basic conversion" $ do
    it "convert variable indices" $ do
      namelessExpr (M.Lambda "x" (M.Variable "x")) `shouldBe` N.Lambda (N.ParamBound 0)
      namelessExpr (M.Lambda "x" (M.Lambda "y" (M.Variable "x"))) `shouldBe` N.Lambda (N.Lambda (N.ParamBound 1))

    it "convert shadowing names" $ do
      namelessExpr (M.Lambda "x" (M.Lambda "x" (M.Variable "x"))) `shouldBe` N.Lambda (N.Lambda (N.ParamBound 0))

  describe "global names" $ do
    it "convert global name reference" $ do
      N.nameless [M.Name (M.NameDef "a" $ M.Integer 1), M.Name (M.NameDef "main" $ M.Variable "a")] `shouldBe` N.Code Map.empty [] [N.Name "a" $ N.Integer 1, N.Name "main" $ N.GlobalBound "a" 0]

  describe "complex examples" $ do
    it "complex example 1" $ do
      namelessExpr (M.Lambda "z" (
                    M.Apply (
                      M.Lambda "y" (
                        M.Apply (M.Variable "y") (M.Lambda "x" (M.Variable "x"))
                      )
                    )
                    (
                      M.Lambda "x" (
                        M.Apply (M.Variable "z") (M.Variable "x")
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
      namelessExpr (M.Lambda "f" (
                    M.Apply (
                      M.Lambda "x" (
                        M.Apply
                          (M.Variable "f")
                          (M.Lambda "y" (
                            M.Apply (M.Apply (M.Variable "x") (M.Variable "x")) (M.Variable "y")
                          ))
                      )
                    )
                    (
                      M.Lambda "x" (
                        M.Apply
                          (M.Variable "f")
                          (M.Lambda "y" (
                            M.Apply (M.Apply (M.Variable "x") (M.Variable "x")) (M.Variable "y")
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
