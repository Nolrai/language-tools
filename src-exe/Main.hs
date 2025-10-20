{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Main where

import Data.Text
import Data.Text.IO hiding (writeFile)
import System.IO (IO, FilePath)
import System.Environment
import System.Exit
import MyLib (parseFile, intoSCA2, intoLexurgy)
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
  putStrLn $ "writing file " <> prefix <> ".sca2"
  writeFile (unpack (prefix <> ".sca2")) $ intoSCA2 result
  MyLib.LexurgyExport.writeLexurgyFeatures (unpack (prefix <> ".sc"))
  putStrLn $ "writing file " <> prefix <> ".wl"
  writeFile (unpack (prefix <> ".wl")) $ intoLexurgy result
  exitSuccess
