{-# LANGUAGE OverloadedStrings #-}
module Test.IOUtf8 (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Text as T
import Utils.IO (readFileUtf8, writeFileUtf8)
import System.IO.Temp (withSystemTempFile)
import qualified Data.ByteString as BS

tests :: TestTree
tests = testGroup "Utils.IO"
  [ testCase "read/write utf8" $ withSystemTempFile "u.txt" $ \fp h -> do
      writeFileUtf8 fp ("héllo" :: T.Text)
      t <- readFileUtf8 fp
      assertEqual "utf8 roundtrip" ("héllo" :: T.Text) t
  , testCase "invalid utf8 fails" $ withSystemTempFile "bad.txt" $ \fp h -> do
      BS.writeFile fp (BS.pack [0xff,0xfe,0xff])
      err <- try (readFileUtf8 fp) :: IO (Either SomeException T.Text)
      case err of
        Left _ -> pure ()
        Right _ -> assertFailure "expected decode failure"
  ]