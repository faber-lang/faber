module Main where

import Compile

import qualified Data.ByteString.Char8 as BS
import           System.Environment
import           System.IO

main :: IO ()
main = do
  filename <- (!! 0) <$> getArgs
  content <- readFile filename
  case compileToModule filename content of
    Right e  -> BS.putStrLn =<< moduleToLLVMAssembly e
    Left err -> hPrint stderr err
