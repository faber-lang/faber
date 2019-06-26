{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Integration (spec) where

import           Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy  as BS
import           Data.FileEmbed
import           System.IO
import           System.IO.Temp
import           System.Process.Typed
import           Test.Hspec

import Compile

execBS :: BSC.ByteString -> IO BS.ByteString
execBS = execStr . BSC.unpack

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
    it "arithmetic" $ do
      execExpr "40 + 3" `shouldReturn` "43\n"
      execExpr "12 * 3" `shouldReturn` "36\n"
      execExpr "10 - 2" `shouldReturn` "8\n"
    it "function application" $ execExpr "(\\x=>x) 42" `shouldReturn` "42\n"
    it "passing function" $ execExpr "(\\f x => f (f x)) (\\x => x+1) 3" `shouldReturn` "5\n"
    it "many arguments" $ execExpr "(\\a b c d => a + b + c + d) 1 2 3 4" `shouldReturn` "10\n"

    it "let-in and where" $ execBS $(embedFile "test/data/let_in.fab") `shouldReturn` "47\n"
    it "church numerals" $ execBS $(embedFile "test/data/church.fab") `shouldReturn` "15\n"
    it "let polymorphism" $ execBS $(embedFile "test/data/let_poly.fab") `shouldReturn` "4\n"
    it "annotation" $ execBS $(embedFile "test/data/annotation.fab") `shouldReturn` "10\n"

    it "conditional" $ execBS $(embedFile "test/data/if_then_else.fab") `shouldReturn` "43\n"
