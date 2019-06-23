module Main where

import Compile
import Parse

import qualified Data.Text.IO       as TIO
import           System.Environment

main :: IO ()
main = do
  filename <- (!! 0) <$> getArgs
  content <- readFile filename
  case parseCode filename content of
    Right e -> do
      ir <- compile e
      TIO.putStrLn ir
    Left (ParseError err) -> Prelude.putStrLn err
