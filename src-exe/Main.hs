{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Main where

import Data.Text
import Data.Text.IO hiding (writeFile)
import System.IO (IO, FilePath)
import System.Environment
import System.Exit
import MyLib (parseFile, intoSCA2)
import Prelude ((==))
import Data.ByteString (writeFile)
import Data.Function (($))
import Data.Monoid ((<>))

main :: IO ()
main = do
  [fileName :: FilePath] <- getArgs
  let (prefix, _) = break (== '.') (pack fileName)
  putStrLn $ "reading file: " <> pack fileName
  result <- parseFile fileName
  putStrLn $ "writing file " <> prefix <> ".utf8"
  writeFile (unpack (prefix <> ".utf8")) $ intoSCA2 result
  exitSuccess
