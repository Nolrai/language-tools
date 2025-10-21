{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Main where

import Data.Text
import Data.Text.IO hiding (writeFile)
import Data.Text.Encoding (encodeUtf8)
import System.IO (IO, FilePath)
import System.Environment
import System.Exit (exitSuccess)
import MyLib (parseFile, intoSCA2, intoLexurgy, lexurgyPrelude)
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

  putStrLn "parsing complete."
  putStrLn $ "writing files with prefix: " <> prefix
  putStrLn "writing sca files..."
  putStrLn $ "writing file " <> prefix <> ".sca2"
  writeFile (unpack (prefix <> ".sca2")) $ intoSCA2 result

  putStrLn "writing lexurgy files..."
  putStrLn $ "writing lexurgy definitions to: " <> prefix <> ".sc"
  writeFile (unpack (prefix <> ".sc")) (encodeUtf8 lexurgyPrelude)
  putStrLn $ "writing word list to: " <> prefix <> ".wl"
  writeFile (unpack (prefix <> ".wl")) $ intoLexurgy result
  exitSuccess
