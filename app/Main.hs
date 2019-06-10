module Main where

import Compile
import Parse

import Data.Text.IO       as TIO
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  case parseExpr "input" (args !! 0) of
    Right e -> do
      (_ty, ir) <- compile e
      TIO.putStrLn ir
    Left (ParseError err) -> Prelude.putStrLn err
