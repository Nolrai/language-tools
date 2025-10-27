{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Data.Map.Strict as Map
import Data.Set as Set

import Data.Function (($), (.))
import Data.Monoid ((<>))
import Data.Text as T
import Data.Text.IO hiding (writeFile)
import Data.ByteString as BS
import HumanLanguage.IPANormalize (normalizeIpaText)
import System.Environment
import System.Exit (exitSuccess, ExitCode (..), exitWith)
import System.IO (IO, putStr)
import System.FilePath (FilePath, takeExtension, takeBaseName)
import System.Directory (withCurrentDirectory, getDirectoryContents)
import Prelude ( (==), Eq(..), Ord)
import Control.Exception (handle, SomeException, throwIO, Exception, displayException, catch)
import Control.Monad ((=<<), forM, mapM)
import Control.Applicative ((<$>), Applicative (..))
import qualified Data.List as List
import HumanLanguage.Entry (Entry(..))
import qualified Data.Text as Text
import Control.Arrow (second)
import Data.Tuple (snd)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Either (Either(..))
import HumanLanguage.LexurgyExport
import GHC.Stack (HasCallStack)
import Data.Traversable (for)
import Data.Foldable (mapM_)

import RunLexurgy (runLexurgy)
import Data.Typeable (Typeable)

-- | Lightweight exception wrapper that annotates an arbitrary exception with context.
newtype AnnotatedException = AnnotatedException String
  deriving (Show, Typeable)

instance Exception AnnotatedException where
  displayException (AnnotatedException s) = s

-- | Run an IO action and, on exception, rethrow it wrapped with a short context message.
annotateIO :: String -> IO a -> IO a
annotateIO ctx action =
  action `catch` \ (e :: SomeException) ->
    throwIO (AnnotatedException (ctx <> ": " <> displayException e))

main :: HasCallStack => IO ()
main = do
  handle exceptionHandler body
  putStrLn "All done"
  exitSuccess

body :: HasCallStack => IO ()
body = do
  [wordListDir, rulesDir, workingDir, outputDir] :: HasCallStack => [FilePath] <- getArgs

  (simpleWordLists :: HasCallStack => [(filePath, [(Text, Text)])]) <- withCurrentDirectory wordListDir $ do
    (fileNames :: HasCallStack => [FilePath]) <- getDirectoryContents "."
    let inputFiles = List.filter (\f -> takeExtension f == ".swl") fileNames
    forM inputFiles $ \fileName -> do
      putStrLn $ "reading simple word list file: " <> T.pack fileName
      -- annotate file-read errors with the filename for better diagnostics
      contents <- annotateIO ("reading file " <> fileName) (decodeUtf8 <$> BS.readFile fileName)
      let parts = second normalizeIpaText . Text.breakOn " " <$> lines contents
      pure (takeBaseName fileName, parts)

  (lexurgySoundChanges :: HasCallStack => [(filePath, Text)]) <- withCurrentDirectory rulesDir $ do
    (fileNames :: HasCallStack => [FilePath]) <- getDirectoryContents "."
    let inputFiles = List.filter (\f -> takeExtension f == ".lsc") fileNames
    forM inputFiles $ \fileName -> do
      putStrLn $ "reading lexurgy rule file: " <> T.pack fileName
      contents <- readUtf8File fileName
      pure (takeBaseName fileName, contents)


  putStrLn $ "read " <> show (List.length simpleWordLists) <> " simple word list files."

  -- make dictionary
  let spellingToIpa :: HasCallStack => Map Text (Set Text) = mkSpellingToIpa (snd <$> simpleWordLists)

  putStrLn "parsing complete."

  -- write input files to lexurgy
  withCurrentDirectory workingDir $ do
    let ipas = Set.unions (Map.elems spellingToIpa)
    let ipaFilePath = "all_ipas.txt"
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
    sequence_ $ do
      ruleName <- getBaseName <$> ruleFiles
      let wordFileName = getBaseName ipaFilePat
      let evolvedFileName = wordFileName <> "_" <> ruleName <> ".wli"
      ipaToEvolved <- linesToMap "=>" <$> readUtf8File evolvedFileName

      let spellingToEvolved :: HasCallStack => Map Text (Set Text)
          spellingToEvolved = composeMaps spellingToIpa ipaToEvolved
      let outputFilePath = outputDir <> "/" <> ruleName <> "_evolved_words.txt"
      let outputLines =
            [ spelling <> "\t" <> T.intercalate ", " (Set.toList ipas)
            | (spelling, ipas) <- Map.toList spellingToEvolved
            ]
      putStrLn $ "writing evolved words to: " <> T.pack outputFilePath
      writeFile outputFilePath (encodeUtf8 . unlines $ outputLines)

      let evolvedToSpelling :: HasCallStack => Map Text (Set Text)
          evolvedToSpelling = reverseMap spellingToEvolved
      let reverseOutputFilePath = outputDir <> "/" <> ruleName <> "_evolved_ipas.txt"
      let reverseOutputLines =
            [ ipa <> "\t" <> T.intercalate ", " (Set.toList spellings)
            | (ipa, spellings) <- Map.toList evolvedToSpelling
            ]
      putStrLn $ "writing reverse mapping to: " <> T.pack reverseOutputFilePath
      writeFile reverseOutputFilePath (encodeUtf8 . unlines $ reverseOutputLines)

      putStrLn $ "completed processing for rule: " <> T.pack ruleName

linesToMap :: (HasCallStack, MonadFail m) => String -> Text -> m (Map Text (Set Text))
linesToMap seperator contents = do
  pairs <- for (lines contents) $ \line -> do
    let (spelling, rest) = Text.breakOn (T.pack seperator) line
    when (T.null rest) . fail
      $ "Invalid line (missing separator '" <> seperator <> "'): " <> T.unpack line
    let ipa = T.strip (T.drop (T.length (T.pack seperator)) rest)
    pure (strip spelling, Set.singleton ipa)
  pure $ Map.fromListWith Set.union pairs

mkSpellingToIpa :: HasCallStack => [[(Text, Text)]] -> Map Text (Set Text)
mkSpellingToIpa list = unionsWith Set.union $ toMap <$> list
  where
    toMap :: HasCallStack => [(Text, Text)] -> Map Text (Set Text)
    toMap entries =
      fromListWith Set.union
        [ (spelling, Set.singleton ipa)
        | (spelling, ipa) <- entries ]

exceptionHandler :: HasCallStack => SomeException -> IO ()
exceptionHandler ex = do
  putStrLn $ "An error occurred: " <> show ex
  putStrLn "Usage: Main <input-file> <output-directory>"
  putStrLn . ("getArgs returned: " <>) . show =<< getArgs

  exitWith (ExitFailure 1)

sortAndNub :: HasCallStack => Ord a => [a] -> [a]
sortAndNub = Set.toList . Set.fromList

readUtf8File :: HasCallStack => FilePath -> IO Text
readUtf8File path =
  -- annotate IO errors with the path to help trace failures
  annotateIO ("reading file " <> path) (decodeUtf8 <$> BS.readFile path)

toSpelling :: HasCallStack => Entry -> Text
toSpelling Entry {entryRank, entrySpelling} = show entryRank <> "\t\t" <> entrySpelling