{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Data.ByteString (writeFile)
import Data.Function (($))
import Data.Monoid ((<>))
import Data.Text
import Data.Text.Encoding (encodeUtf8)
import Data.Text.IO hiding (writeFile)
import HumanLanguage.EntryParser (parseFile)
import HumanLanguage.LexurgyExport (lexurgyDefaultRules, lexurgyPrelude)
import HumanLanguage.PrintToLexurgy (intoLexurgy)
import HumanLanguage.PrintToSCA2 (intoSCA2)
import System.Environment
import System.Exit (exitSuccess)
import System.IO (FilePath, IO)
import Prelude ((==))

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
  let lscContents = encodeUtf8 $ lexurgyPrelude <> "\n" <> lexurgyDefaultRules
  writeFile (unpack (prefix <> ".lsc")) lscContents
  putStrLn $ "writing word list to: " <> prefix <> ".wl"
  writeFile (unpack (prefix <> ".wli")) $ intoLexurgy result
  exitSuccess
