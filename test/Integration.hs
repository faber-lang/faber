{-# LANGUAGE OverloadedStrings #-}

module Integration (spec) where

import qualified Data.ByteString.Lazy as BS
import           System.IO
import           System.IO.Temp
import           System.Process.Typed
import           Test.Hspec

import Compile

execExpr :: String -> IO BS.ByteString
execExpr expr = execStr $ "name main = " ++ expr

execStr :: String -> IO BS.ByteString
execStr content = do
  case compileToModule "" content of
    Right m ->
      withSystemTempFile "fabertest.ll" $ \path handle -> do
        BS.hPut handle =<< BS.fromStrict <$> moduleToLLVMAssembly m
        hClose handle
        fst <$> readProcess_ (shell $ "lli " ++ path)
    Left err -> error $ show err

spec :: Spec
spec = do
  describe "Integration Tests" $ do
    it "return int" $ execExpr "10" `shouldReturn` "10\n"
    it "function application" $ execExpr "(\\x=>x) 42" `shouldReturn` "42\n"
    it "passing function" $ execExpr "(\\f x => f (f x)) (\\x => x+1) 3" `shouldReturn` "5\n"
    it "many arguments" $ execExpr "(\\a b c d => a + b + c + d) 1 2 3 4" `shouldReturn` "10\n"
