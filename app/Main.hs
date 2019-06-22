module Main where

import Compile
import Parse

import Data.Text.IO       as TIO
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  case parseCode "input" (args !! 0) of
    Right e -> do
      ir <- compile e
      TIO.putStrLn ir
    Left (ParseError err) -> Prelude.putStrLn err
