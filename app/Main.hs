module Main where

import Text.Megaparsec
import Parse
import Eval

main :: IO ()
main = do
  result <- parse parser "" <$> getLine
  case result of
    Right e -> print (eval e) >> main
    Left err -> print err
