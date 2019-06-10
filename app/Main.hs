module Main where

import Parse
import Compile

import System.Environment
import Data.Text.IO as TIO

main :: IO ()
main = do
  args <- getArgs
  case parse_expr "input" (args !! 0) of
    Right e -> do
      (_ty, ir) <- compile e
      TIO.putStrLn ir
    Left (ParseError err) -> Prelude.putStrLn err
