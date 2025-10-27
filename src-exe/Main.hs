{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Control.Applicative (Applicative (..), (<$>))
import Control.Arrow (second)
import Control.Exception (SomeException, handle)
import Control.Monad (MonadFail, fail, forM, when, (=<<))
import Data.ByteString as BS
import Data.Dictionary.Utils (composeDicts, reverseDict, (:=>))
import Data.Foldable (mapM_, for_)
import Data.Function (($), (.))
import Data.List qualified as List
import Data.Map.Strict as Map
import Data.Monoid ((<>))
import Data.Set as Set
import Data.Text as T
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Text.IO hiding (writeFile)
import Data.Traversable (for)
import Data.Tuple (snd)
import Data.Typeable ()
import GHC.Stack (HasCallStack)
import HumanLanguage.Entry (Entry (..))
import HumanLanguage.IPANormalize (normalizeIpaText)
import HumanLanguage.LexurgyExport
import System.Directory (getDirectoryContents, withCurrentDirectory)
import System.Environment
import System.Exit (ExitCode (..), exitSuccess, exitWith)
import System.FilePath (FilePath, takeBaseName, takeExtension)
import System.IO (IO)
import Utils.IO (annotateIO, runLexurgy)
import Prelude (Eq (..), Ord, (==))

main :: IO ()
main = do
  handle exceptionHandler body
  putStrLn "All done"
  exitSuccess

body :: IO ()
body = do
  [wordListDir, rulesDir, workingDir, outputDir] :: (HasCallStack) => [FilePath] <- getArgs

  (simpleWordLists :: (HasCallStack) => [(filePath, [(Text, Text)])]) <- withCurrentDirectory wordListDir $ do
    (fileNames :: (HasCallStack) => [FilePath]) <- getDirectoryContents "."
    let inputFiles = List.filter (\f -> takeExtension f == ".swl") fileNames
    forM inputFiles $ \fileName -> do
      putStrLn $ "reading simple word list file: " <> T.pack fileName
      -- annotate file-read errors with the filename for better diagnostics
      contents <- annotateIO ("reading file " <> T.pack fileName) (decodeUtf8 <$> BS.readFile fileName)
      let parts = second normalizeIpaText . Text.breakOn " " <$> lines contents
      pure (takeBaseName fileName, parts)

  (lexurgySoundChanges :: (HasCallStack) => [(filePath, Text)]) <- withCurrentDirectory rulesDir $ do
    (fileNames :: (HasCallStack) => [FilePath]) <- getDirectoryContents "."
    let inputFiles = List.filter (\f -> takeExtension f == ".lsc") fileNames
    forM inputFiles $ \fileName -> do
      putStrLn $ "reading lexurgy rule file: " <> T.pack fileName
      contents <- readUtf8File fileName
      pure (takeBaseName fileName, contents)

  putStrLn $ "read " <> show (List.length simpleWordLists) <> " simple word list files."

  -- make dictionary
  let spellingToIpa :: (HasCallStack) => Map Text (Set Text) = mkSpellingToIpa (snd <$> simpleWordLists)

  putStrLn "parsing complete."

  -- write input files to lexurgy
  withCurrentDirectory workingDir $ do
    do
      -- write all_ipas.txt
      let ipas = Set.unions (Map.elems spellingToIpa)
      writeFile ipaFilePath (encodeUtf8 . unlines $ Set.toList ipas)
      putStrLn $ "wrote all_ipas.txt with " <> show (Set.size ipas) <> " unique IPA entries."

    -- write rule files
    ruleFiles <- for lexurgySoundChanges $ \(fileBaseName, contents) -> do
      let ruleFileName = fileBaseName <> ".lsc"
      writeFile ruleFileName (encodeUtf8 . unlines $ [lexurgyPrelude, contents])
      putStrLn $ "wrote lexurgy rule file: " <> T.pack ruleFileName
      pure ruleFileName
    -- run lexurgy
    runLexurgy [ipaFilePath] `mapM_` ruleFiles

    -- read lexurgy outputs and output final results
    for_ ruleFiles $ \ruleFileName -> do
      let ruleName = takeBaseName ruleFileName
          wordFileName = takeBaseName ipaFilePath
          evolvedFileName = wordFileName <> "_" <> ruleName <> ".wli"
      fileContents <- readUtf8File evolvedFileName
      ipaToEvolved :: (HasCallStack) => Text :=> Text
        <- linesToDict "=>" . normalizeIpaText $ fileContents
      let spellingToEvolved :: (HasCallStack) => Text :=> Text
          spellingToEvolved = composeDicts spellingToIpa ipaToEvolved
          outputFilePath = outputDir <> "/" <> ruleName <> "_evolved_words.txt"
          outputLines =
            [ spelling <> "\t" <> T.intercalate ", " (Set.toList ipas)
            | (spelling, ipas) <- Map.toList spellingToEvolved
            ]
      putStrLn $ "writing evolved words to: " <> T.pack outputFilePath
      writeFile outputFilePath (encodeUtf8 . unlines $ outputLines)

      let evolvedToSpelling :: (HasCallStack) => Text :=> Text
          evolvedToSpelling = reverseDict spellingToEvolved
      let reverseOutputFilePath = outputDir <> "/" <> ruleName <> "_evolved_ipas.txt"
      let reverseOutputLines =
            [ ipa <> "\t" <> T.intercalate ", " (Set.toList spellings)
            | (ipa, spellings) <- Map.toList evolvedToSpelling
            ]
      putStrLn $ "writing reverse mapping to: " <> T.pack reverseOutputFilePath
      writeFile reverseOutputFilePath (encodeUtf8 . unlines $ reverseOutputLines)

      putStrLn $ "completed processing for rule: " <> T.pack ruleName
  where
    ipaFilePath = "all_ipas.txt"

linesToDict :: (MonadFail m) => Text -> Text -> m (Text :=> Text)
linesToDict seperator contents = do
  pairs <- for (lines contents) $ \line -> do
    let (spelling, rest) = Text.breakOn seperator line
    when (T.null rest) . fail . T.unpack $
      "Invalid line (missing separator '" <> seperator <> "'): " <> line
    let ipa = T.strip (T.drop (T.length seperator) rest)
    pure (strip spelling, Set.singleton ipa)
  pure $ Map.fromListWith Set.union pairs

mkSpellingToIpa :: [[(Text, Text)]] -> Map Text (Set Text)
mkSpellingToIpa list = unionsWith Set.union $ toMap <$> list
  where
    toMap :: [(Text, Text)] -> Map Text (Set Text)
    toMap entries =
      fromListWith
        Set.union
        [ (spelling, Set.singleton ipa)
        | (spelling, ipa) <- entries
        ]

exceptionHandler :: SomeException -> IO ()
exceptionHandler ex = do
  putStrLn $ "An error occurred: " <> show ex
  putStrLn "Usage: Main <input-file> <output-directory>"
  putStrLn . ("getArgs returned: " <>) . show =<< getArgs

  exitWith (ExitFailure 1)

sortAndNub :: (Ord a) => [a] -> [a]
sortAndNub = Set.toList . Set.fromList

readUtf8File :: FilePath -> IO Text
readUtf8File path =
  -- annotate IO errors with the path to help trace failures
  annotateIO ("reading file " <> T.pack path) (decodeUtf8 <$> BS.readFile path)

toSpelling :: Entry -> Text
toSpelling Entry {entryRank, entrySpelling} = show entryRank <> "\t\t" <> entrySpelling
