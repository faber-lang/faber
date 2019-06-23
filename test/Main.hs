module Main where

import Test.Hspec.Runner

import qualified Integration
import qualified Unit

main :: IO ()
main = hspecWith defaultConfig {configFormatter = Nothing} $ do
  Unit.spec
  Integration.spec
