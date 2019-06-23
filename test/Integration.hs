{-# LANGUAGE OverloadedStrings #-}

module Integration (spec) where

import qualified Data.ByteString.Lazy as BS
import           System.IO
import           System.IO.Temp
import           System.Process.Typed
import           Test.Hspec

import Compile

execFile :: String -> IO BS.ByteString
execFile filename = do
  content <- readFile filename
  execStr content

execStr :: String -> IO BS.ByteString
execStr content = do
  case compileToModule "" content of
    Right m ->
      withSystemTempFile "fabertest.ll" $ \path handle -> do
        BS.hPut handle =<< BS.fromStrict <$> moduleToLLVMAssembly m
        hClose handle
        fst <$> (readProcess_ $ shell $ "lli " ++ path)
    Left err -> error $ show err

spec :: Spec
spec = do
  describe "simple" $ do
    it "int" $ do
      execStr "name main = 10" `shouldReturn` "10\n"
