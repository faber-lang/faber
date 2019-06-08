module Main where

import Text.Megaparsec
import Parse
import Compile

import System.Environment
import Data.Text.IO as TIO

main :: IO ()
main = do
  args <- getArgs
  case parse parser "" (args !! 0) of
    Right e -> TIO.putStrLn =<< compile e
    Left err -> print err
