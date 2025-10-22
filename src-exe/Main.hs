{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Data.ByteString (writeFile)
import Data.Function (($), (.))
import Data.Monoid ((<>))
import Data.Text
import Data.Text.Encoding (encodeUtf8)
import Data.Text.IO hiding (writeFile)
import HumanLanguage.EntryParser (parseFile)
import HumanLanguage.LexurgyExport (lexurgyPrelude)
import HumanLanguage.PrintToLexurgy (intoLexurgy)
import System.Environment
import System.Exit (exitSuccess, ExitCode (..), exitWith)
import System.IO (IO)
import System.FilePath (FilePath, takeExtension, takeBaseName)
import System.Directory (withCurrentDirectory, createDirectoryIfMissing, getDirectoryContents)
import Prelude ( (==), Bool(..), Eq(..), fst, print)
import Control.Exception (handle, SomeException)
import Control.Monad ((=<<), forM, forM_)
import Control.Applicative ((<$>), Applicative (..))
import qualified Data.List as List
import HumanLanguage.Entry (Entry)
import System.Process (callProcess)

main :: IO ()
main = handle exceptionHandler $ do
  [wordListDir, rulesDir, outputDir] :: [FilePath] <- getArgs

  (wordLists :: [(FilePath, [Entry])]) <- withCurrentDirectory wordListDir $ do
    (fileNames :: [FilePath]) <- getDirectoryContents "."
    let inputFiles = List.filter (\f -> takeExtension f == ".csv") fileNames
    forM inputFiles $ \fileName -> do
      putStrLn $ "reading file: " <> pack fileName
      (takeBaseName fileName,) <$> parseFile fileName

  (rules :: [(FilePath, Text)]) <- withCurrentDirectory rulesDir $ do
    (fileNames :: [FilePath]) <- getDirectoryContents "."
    let ruleFiles = List.filter (\f -> takeExtension f == ".lsc") fileNames
    forM ruleFiles $ \fileName -> do
      putStrLn $ "reading rules file: " <> pack fileName
      content <- readFile fileName
      pure (takeBaseName fileName, content)

  putStrLn "parsing complete."

  createDirectoryIfMissing True outputDir
  _ <- withCurrentDirectory outputDir $ do

    putStrLn "writing lexurgy word list files..."
    forM_ wordLists $ \(wordListPrefix, entries) -> do
      putStrLn $ "writing word list to: " <> pack wordListPrefix <> ".wl"
      writeFile (wordListPrefix <> ".wli") $ intoLexurgy entries

    putStrLn "writing rules files..."
    forM_ rules $ \(rulesPrefix, rulesContent) -> do
      let rulesFileName = rulesPrefix <> ".lsc"
      putStrLn $ "writing lexurgy rules file: " <> pack rulesFileName
      writeFile rulesFileName (encodeUtf8 (lexurgyPrelude <> rulesContent))

    putStrLn "calling lexurgy to run .lsc files on .wl files..."
    forM_ rules $ \(rulesPrefix, _) -> do
      let rulesFileName = rulesPrefix <> ".lsc"
      putStrLn $ "running rules file: " <> pack rulesFileName
      let wordListFiles = (<> ".wl") . fst <$> wordLists
      let arguments = ["sc", "--out-suffix", "_rulesPrefix", rulesFileName] <> wordListFiles
      putStrLn "about to call: lexurgy with arguments:"
      print arguments
      callProcess lexurgyPath arguments

  putStrLn "All done."
  exitSuccess

exceptionHandler :: SomeException -> IO ()
exceptionHandler ex = do
  putStrLn $ "An error occurred: " <> show ex
  putStrLn "Usage: Main <input-file> <output-directory>"
  putStrLn . ("getArgs returned: " <>) . show =<< getArgs

  exitWith (ExitFailure 1)

lexurgyPath :: FilePath
lexurgyPath = "/home/chris/myprojects/language-tools/lexurgy"