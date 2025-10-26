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
import HumanLanguage.LexurgyExport (lexurgyPrelude, lexurgyPath)
import HumanLanguage.PrintToLexurgy (intoLexurgy)
import HumanLanguage.IPANormalize (normalizeIpaText)
import System.Environment
import System.Exit (exitSuccess, ExitCode (..), exitWith)
import System.IO (IO)
import System.FilePath (FilePath, takeExtension, takeBaseName)
import System.Directory (withCurrentDirectory, createDirectoryIfMissing, getDirectoryContents)
import Prelude ( (==), Bool(..), Eq(..), fst, print, Ord)
import Control.Exception (handle, SomeException)
import Control.Monad ((=<<), forM, forM_)
import Control.Applicative ((<$>), Applicative (..))
import qualified Data.List as List
import System.Process (callProcess)
import HumanLanguage.Entry (Entry(..))
import Data.Set as Set
import qualified Data.Text as Text
import Control.Arrow (second)

main :: IO ()
main = do
  handle exceptionHandler body
  putStrLn "All done"
  exitSuccess


body :: IO ()
body = do
  [wordListDir, rulesDir, outputDir] :: [FilePath] <- getArgs

  (wordLists :: [(FilePath, [Entry])]) <- withCurrentDirectory wordListDir $ do
    (fileNames :: [FilePath]) <- getDirectoryContents "."
    let inputFiles = List.filter (\f -> takeExtension f == ".csv") fileNames
    forM inputFiles $ \fileName -> do
      putStrLn $ "reading file: " <> pack fileName
      (takeBaseName fileName,) <$> parseFile fileName

  (simpleWordLists :: [(filePath, [(Text, Text)])]) <- withCurrentDirectory wordListDir $ do
    (fileNames :: [FilePath]) <- getDirectoryContents "."
    let inputFiles = List.filter (\f -> takeExtension f == ".swl") fileNames
    forM inputFiles $ \fileName -> do
      putStrLn $ "reading simple word list file: " <> pack fileName
      lines' <- lines <$> readFile fileName
      let parts = second normalizeIpaText . Text.breakOn " " <$> lines'
      pure (takeBaseName fileName, parts)

  (rules :: [(FilePath, Text)]) <- withCurrentDirectory rulesDir $ do
    (fileNames :: [FilePath]) <- getDirectoryContents "."
    let ruleFiles = List.filter (\f -> takeExtension f == ".lsc") fileNames
    forM ruleFiles $ \fileName -> do
      putStrLn $ "reading rules file: " <> pack fileName
      content <- readFile fileName
      pure (takeBaseName fileName, content)

  putStrLn "parsing complete."

  createDirectoryIfMissing True outputDir
  withCurrentDirectory outputDir $ do

    putStrLn "writing spelling list"
    forM_ wordLists $ \(wordListPrefix, entries) -> do
      putStrLn $ "writing spelling list: " <> pack wordListPrefix <> ".txt"
      writeFile (wordListPrefix <> ".txt") . encodeUtf8 . unlines . sortAndNub $ toSpelling <$> entries

    putStrLn "writing lexurgy word list files..."
    forM_ wordLists $ \(wordListPrefix, entries) -> do
      putStrLn $ "writing word list to: " <> pack wordListPrefix <> ".wli"
      writeFile (wordListPrefix <> ".wli") $ intoLexurgy entries
    forM_ simpleWordLists $ \(wordListPrefix, entries) -> do
      putStrLn $ "writing simple word list to: " <> pack wordListPrefix <> ".wli"
      let txt = Text.intercalate "\n" [ipa <> "\t" <> gloss | (ipa, gloss) <- entries]
      writeFile (wordListPrefix <> ".wli") $ encodeUtf8 txt

    putStrLn "writing rules files..."
    forM_ rules $ \(rulesPrefix, rulesContent) -> do
      let rulesFileName = rulesPrefix <> ".lsc"
      putStrLn $ "writing lexurgy rules file: " <> pack rulesFileName
      writeFile rulesFileName (encodeUtf8 (lexurgyPrelude <> rulesContent))

    putStrLn "calling lexurgy to run .lsc files on .wl files..."
    forM_ rules $ \(rulesPrefix, _) -> do
      let rulesFileName = rulesPrefix <> ".lsc"
      putStrLn $ "running rules file: " <> pack rulesFileName
      let wordListFiles = (<> ".wli") <$> (fst <$> wordLists) <> (fst <$> simpleWordLists)
      let arguments = ["sc", "--out-suffix", "_ev", rulesFileName] <> wordListFiles
      putStrLn "about to call: lexurgy with arguments:"
      print arguments
      callProcess lexurgyPath arguments

exceptionHandler :: SomeException -> IO ()
exceptionHandler ex = do
  putStrLn $ "An error occurred: " <> show ex
  putStrLn "Usage: Main <input-file> <output-directory>"
  putStrLn . ("getArgs returned: " <>) . show =<< getArgs

  exitWith (ExitFailure 1)

sortAndNub :: Ord a => [a] -> [a]
sortAndNub = Set.toList . Set.fromList

toSpelling :: Entry -> Text
toSpelling Entry {entryRank, entrySpelling} = show entryRank <> "\t\t" <> entrySpelling