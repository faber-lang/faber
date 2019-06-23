module Main where

import Compile
import Parse

import qualified Data.ByteString    as BS
import           System.Environment

main :: IO ()
main = do
  filename <- (!! 0) <$> getArgs
  content <- readFile filename
  case compileToModule filename content of
    Right e  -> BS.putStrLn =<< moduleToLLVMAssembly e
    Left err -> print err
