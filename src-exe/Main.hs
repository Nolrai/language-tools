{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Main executable for the language-tools pipeline.
-- |
-- | Responsibilities:
-- |  * Read simple word lists and lexurgy rule files from the provided input
-- |    directories.
-- |  * Normalize IPA strings and prepare an IPA list for the `lexurgy` tool.
-- |  * Run `lexurgy` for each rule file and collect the evolved word mappings.
-- |  * Write per-rule outputs (spelling -> evolved IPAs and the reverse).
-- |
-- | The program expects four positional arguments:
-- |   1. wordListDir   -- directory containing `.swl` word list files
-- |   2. rulesDir      -- directory containing `.lsc` lexurgy rule files
-- |   3. workingDir    -- directory used to stage intermediate files
-- |   4. outputDir     -- directory to write final evolved mappings
module Main (main) where

import Control.Applicative (Applicative (..), (<$>))
import Control.Arrow (second)
import Control.Exception (Exception (displayException), SomeException, handle, throwIO)
-- import HumanLanguage.LexurgyExport

import Control.Monad (unless, when, (=<<), guard)
import Data.Bool ( Bool(..), not, otherwise )
import Data.ByteString as BS ( readFile, writeFile )
import Data.Dictionary.Utils (linesToDict, reverseDict, (:=>), composeAndFindMissing)
import Data.Eq (Eq (..))
import Data.Foldable (for_, mapM_)
import Data.Function (($), (.))
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Monoid ((<>))
import Data.Ord ( Ord((>), (<)), Down(Down) )
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text as T
    ( Text,
      intercalate,
      lines,
      null,
      show,
      strip,
      unlines,
      pack,
      unpack, elem )
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Text.IO ( putStrLn, hPutStrLn )
import Data.Traversable (for, forM)
import Data.Tuple (snd)
import Data.Typeable ()
import GHC.Stack (HasCallStack)
import HumanLanguage.IPA (isIPAChar)
import HumanLanguage.IPANormalize (normalizeIpaText)
import System.Directory (createDirectoryIfMissing, getDirectoryContents, withCurrentDirectory)
import System.Environment ( getArgs )
import System.Exit (ExitCode (..), exitSuccess, exitWith)
import System.FilePath (FilePath, takeBaseName, takeExtension)
import System.IO (IO, stderr)
import System.IO.Error (userError)
import Utils.IO (annotateIO, runLexurgy, errorIO)
import Data.Maybe ( Maybe(..), maybe)

-- | Program entry point.
main :: IO ()
main = do
  handle exceptionHandler body
  putStrLn "All done"
  exitSuccess

-- -- errorKeys -- for debug
-- errorKeys :: [Text]
-- errorKeys = ["AND","ARE","BE","DO","DUE","EAR","EAT","FOR","HE","HER","IT","KEY","KNEE","NEW","OR","SEA","SEE","TEA","TO","TOO","TWO","WHO","WOULD"]

-- keyToKeyValueText :: Text :=> Text -> Text -> Text
-- keyToKeyValueText map (key :: Text) = "(" <> key <> ", " <> valuePart <> ")"
--   where
--     valuePart :: Text
--     valuePart = maybe "N/A" showTextSet $ key `Map.lookup` map

showTextSet :: Set Text -> Text
showTextSet s
  | Set.null s = "[]"
  | otherwise = "['" <> Text.intercalate "', '" (Set.toList s) <> "']"

-- \|
--  Run the main processing pipeline for evolving word lists using sound-change
--  rules and write the results to disk.
--
--  This IO action expects four command-line arguments:
--    1. wordListDir  - directory containing ".swl" simple word list files
--    2. rulesDir     - directory containing ".lsc" lexurgy sound-change rule files
--    3. workingDir   - directory used for temporary/working files during processing
--    4. outputDir    - directory where final output files are written
--
--  High-level behaviour:
--    - Read all ".swl" files from the wordListDir. Each file is interpreted as UTF-8
--      text and parsed into (spelling, ipa) pairs; IPA texts are normalized before use.
--      File-read errors are annotated with the filename to improve diagnostics.
--    - Read all ".lsc" rule files from the rulesDir as UTF-8 text and associate each
--      rule's basename with its file contents.
--    - Build a mapping from spelling to the set of IPA transcriptions found across
--      the loaded simple word lists.
--    - Ensure the workingDir exists and invoke the core processing function
--      (doWorkInWorkingDir) to apply each rule to the dictionary, producing for each
--      rule both:
--        * a mapping from spelling -> set of evolved IPAs
--        * a mapping from evolved IPA -> set of spellings
--    - For each rule, write two tab-separated output files into outputDir:
--        <rule>_evolved_words.txt  : "spelling<TAB>comma-separated-IPAs" per line
--        <rule>_evolved_ipas.txt   : "ipa<TAB>comma-separated-spellings" per line
--      The output directory is created if missing. Progress messages are emitted to
--      stdout.
--
--  Notes and guarantees:
--    - The function prints progress and diagnostic messages to standard output as it
--      reads files and writes results.
--    - IO-related exceptions are not swallowed; HasCallStack annotations are present
--      on intermediate bindings to provide better error locations in traces.
--    - The exact parsing and normalization behavior for IPA and the details of the
--      sound-change application are delegated to helper functions (e.g. normalizeIpaText
--      and doWorkInWorkingDir) and are not duplicated here.
--

body :: IO ()
body = do
  [wordListDir, rulesDir, workingDir, outputDir] :: (HasCallStack) => [FilePath] <- getArgs

  -- read simple word lists
  (simpleWordLists :: (HasCallStack) => [(filePath, [(Text, Text)])]) <- withCurrentDirectory wordListDir $ do
    (fileNames :: (HasCallStack) => [FilePath]) <- getDirectoryContents "."
    let inputFiles = List.filter (\f -> takeExtension f == ".swl") fileNames
    forM inputFiles $ \fileName -> do
      putStrLn $ "reading simple word list file: " <> T.pack fileName
      -- annotate file-read errors with the filename for better diagnostics
      (contents :: (HasCallStack) => Text) <- annotateIO ("reading file " <> T.pack fileName) (decodeUtf8 <$> BS.readFile fileName)
      let parts = second normalizeIpaText . Text.break (`T.elem` " \t") <$> lines contents
      pure (takeBaseName fileName, parts)

  putStrLn $ "read " <> show (List.length simpleWordLists) <> " simple word list files."

  -- read lexurgy rule files
  (lexurgySoundChanges :: (HasCallStack) => [(filePath, Text)]) <- withCurrentDirectory rulesDir $ do
    (fileNames :: (HasCallStack) => [FilePath]) <- getDirectoryContents "."
    let inputFiles = List.filter (\f -> takeExtension f == ".lsc") fileNames
    forM inputFiles $ \fileName -> do
      putStrLn $ "reading lexurgy rule file: " <> T.pack fileName
      contents <- readUtf8File fileName
      pure (takeBaseName fileName, contents)

  -- make spellingToIpa dictionary
  spellingToIpa :: (HasCallStack) => Map Text (Set Text) <-
    mkSpellingToIpa (snd <$> simpleWordLists)
  reportEmptyEntries "spellingToIpa" spellingToIpa
  putStrLn "parsing complete."

  -- ensure working directory exists
  createDirectoryIfMissing True workingDir
  -- do main work in working directory
  dicts :: [(FilePath, Text :=> Text, Text :=> Text)] <- applySoundChanges workingDir lexurgySoundChanges spellingToIpa

  putStrLn $ "dicts is size " <> show (List.length dicts)

  for_ dicts $ \(ruleFileName, dict1, dict2) -> do
    putStrLn $ "processed rule file: " <> T.pack ruleFileName
    let sizes = (Map.size dict1, Map.size dict2)
    putStrLn $ "produced " <> show sizes <> " sized dictionaries."

  -- write final results files
  for_ dicts $ \(ruleFileName, spellingToEvolved, evolvedToSpelling) -> do
    let ruleName = takeBaseName ruleFileName
        outputFilePath = outputDir <> "/" <> ruleName <> "_evolved_words.txt"
        outputLines =
          [ spelling <> "\t" <> T.intercalate ", " (Set.toList ipas)
          | (spelling, ipas) <- Map.toList spellingToEvolved
          ]
        reverseOutputFilePath = outputDir <> "/" <> ruleName <> "_evolved_ipas.txt"
        reverseOutputLines =
          [ ipa <> "\t" <> T.intercalate ", " (Set.toList spellings)
          | (ipa, spellings) <- Map.toList evolvedToSpelling
          ]

    createDirectoryIfMissing True outputDir
    putStrLn $ "writing gloss to evolved words dictionary: " <> T.pack outputFilePath
    writeFile outputFilePath (encodeUtf8 . unlines $ outputLines)
    putStrLn $ "writing reverse mapping to: " <> T.pack reverseOutputFilePath
    writeFile reverseOutputFilePath (encodeUtf8 . unlines $ reverseOutputLines)

    putStrLn "writing conflict files for forward mapping..."
    let conflictLines = toConflicts spellingToEvolved
    let conflictFilePath = outputDir <> "/" <> ruleName <> "_conflicts.txt"
    writeFile conflictFilePath (encodeUtf8 . unlines $ conflictLines)

    putStrLn "writing conflict files for reverse mapping..."
    let reverseConflictLines = toConflicts evolvedToSpelling
    let reverseConflictFilePath = outputDir <> "/" <> ruleName <> "_reverse_conflicts.txt"
    writeFile reverseConflictFilePath (encodeUtf8 . unlines $ reverseConflictLines)

    putStrLn $ "completed processing for rule: " <> T.pack ruleName

-- |
-- Apply a set of Lexurgy sound‑change rules to a spelling-to-IPA dictionary.
--
-- This function performs the following steps in the given working directory:
--
-- - Writes an IPA input file named "all_ipas.txt". The contents are the union of
--   all IPA sets found in the provided `spellingToIpa` dictionary. The file is
--   written as UTF‑8 with one IPA entry per line.
-- - For each entry in `lexurgySoundChanges` (pair of `fileBaseName` and rule
--   `contents`), writes a Lexurgy rule file named `fileBaseName <> ".lsc"`. Each
--   rule file is created by prepending `lexurgyPrelude` to the provided contents.
-- - Invokes the external Lexurgy runner (`runLexurgy`) on the IPA input file for
--   each rule file.
-- - Reads Lexurgy output files expected to be named
--   "<wordFileName>_<ruleName>.wlm" where `wordFileName` is the base name of the
--   IPA file ("all_ipas") and `ruleName` is the base name of the rule file.
-- - Parses each Lexurgy output using `normalizeIpaText` and `linesToDict "=>"`
--   to obtain an IPA→evolved mapping.
-- - Builds two dictionaries for each rule:
--     * `spellingToEvolved` — the composition of the provided `spellingToIpa`
--       mapping with the IPA→evolved mapping (i.e. spellings → evolved forms).
--     * `evolvedToSpelling` — the reverse/inverse of `spellingToEvolved`.
--
-- Parameters:
-- - workingDir :: FilePath
--     Directory where files are written, Lexurgy is invoked, and outputs are read.
-- - lexurgySoundChanges :: [(FilePath, Text)]
--     List of (base file name, rule contents) used to create ".lsc" files.
-- - spellingToIpa :: Text :=> Text
--     Dictionary mapping spellings to sets of IPA forms.
--
-- Return value:
-- IO [(FilePath, Text :=> Text, Text :=> Text)]
-- A list (one element per rule file) of tuples:
-- - the rule file name (e.g. "foo.lsc"),
-- - the `spellingToEvolved` dictionary (spellings → evolved forms),
-- - the `evolvedToSpelling` dictionary (evolved forms → spellings).
--
-- Side effects and assumptions:
-- - Files are created and read in `workingDir`; the caller is responsible for
--   cleanup if desired.
-- - Uses UTF‑8 encoding for all file writes/reads.
-- - Logs progress messages with `putStrLn`.
-- - Relies on the existence and correct behavior of:
--   `runLexurgy`, `lexurgyPrelude`, `normalizeIpaText`, `linesToDict`,
--   `composeDicts`, and `reverseDict`.
-- - IO exceptions may occur for file/system operations or if the Lexurgy process
--   fails. A `HasCallStack` constraint is present so call sites receive stack
--   traces on failures.
--
-- Implementation details:
-- - The IPA input filename is fixed to "all_ipas.txt".
-- - The Lexurgy output parsing expects dictionary lines delimited by "=>".
-- - Rule files are created with the ".lsc" extension by appending ".lsc" to each
--   provided base name.
-- - The function constructs `spellingToEvolved` by composing the supplied
--   `spellingToIpa` with the IPA→evolved mapping produced by Lexurgy, and then
--   derives `evolvedToSpelling` by reversing that composed dictionary.
applySoundChanges ::
  (HasCallStack) =>
  FilePath ->
  [(FilePath, Text)] ->
  Text :=> Text ->
  IO [(FilePath, Text :=> Text, Text :=> Text)]
applySoundChanges workingDir lexurgySoundChanges spellingToIpa =
  withCurrentDirectory workingDir $ do
    -- write input files to lexurgy

    -- write all_ipas.txt
    let ipas = Set.unions (Map.elems spellingToIpa)
    checkForInvalidIpas ipas
    writeFile ipaFilePath (encodeUtf8 . unlines $ Set.toList ipas)
    putStrLn $ "wrote all_ipas.txt with " <> show (Set.size ipas) <> " unique IPA entries."

    -- write rule files
    ruleFiles <- for lexurgySoundChanges $ \(fileBaseName, contents) -> do
      let ruleFileName = fileBaseName <> ".lsc"
      writeFile ruleFileName (encodeUtf8 . unlines $ [contents])
      putStrLn $ "wrote lexurgy rule file: " <> T.pack ruleFileName
      pure ruleFileName
    -- run lexurgy
    runLexurgy [ipaFilePath] `mapM_` ruleFiles

    -- read lexurgy outputs and compute results
    for ruleFiles $ \ruleFileName -> do
      let ruleName = takeBaseName ruleFileName
          wordFileName = takeBaseName ipaFilePath
          evolvedFileName = wordFileName <> "_" <> ruleName <> ".wlm"
      -- read lexurgy output
      putStrLn $ "reading lexurgy output file: " <> T.pack evolvedFileName
      fileContents <- readUtf8File evolvedFileName
      putStrLn $ "lexurgy produced a file with " <> show (List.length (lines fileContents)) <> " lines."

      -- process into dictionaries
      ipaToEvolved :: (HasCallStack) => Text :=> Text <-
        linesToDict "=>" . normalizeIpaText $ fileContents
      reportEmptyEntries "ipaToEvolved" ipaToEvolved
      putStrLn $ "parsed lexurgy output into IPA to evolved dictionary of size " <> show (Map.size ipaToEvolved) <> "."
      let ipaKeySets = Map.keysSet ipaToEvolved
      let ipaSpellingSets = Set.unions (Map.elems spellingToIpa)
      when (Set.size ipaKeySets < Set.size ipaSpellingSets) $
        hPutStrLn stderr $
          "warning: lexurgy output contains fewer IPA keys ("
            <> show (Set.size ipaKeySets)
            <> ") than input IPAs ("
            <> show (Set.size ipaSpellingSets)
            <> ")"
      when (Set.size ipaSpellingSets < Set.size ipaKeySets) $
        hPutStrLn stderr $
          "warning: lexurgy output contains more IPA keys ("
            <> show (Set.size ipaKeySets)
            <> ") than input IPAs ("
            <> show (Set.size ipaSpellingSets)
            <> ")"
      let spellingToEvolved :: (HasCallStack) => Text :=> Text
          (spellingToEvolved, missing) = composeAndFindMissing spellingToIpa ipaToEvolved
          evolvedToSpelling :: (HasCallStack) => Text :=> Text
          evolvedToSpelling = reverseDict spellingToEvolved
      unless (Set.null missing) $ errorIO $ "Missing keys:" <> showTextSet missing
      reportEmptyEntries "spellingToEvolved" spellingToEvolved
      reportEmptyEntries "evolvedToSpelling" evolvedToSpelling
      pure (ruleFileName, spellingToEvolved, evolvedToSpelling)
  where
    ipaFilePath = "all_ipas.txt"

toConflicts :: Text :=> Text -> [Text]
toConflicts dict =
  let conflicts = Map.toAscList $ Map.filter (\s -> Set.size s > 1) dict
      sortedConflicts = List.sortOn (Down . Set.size . snd) conflicts
   in [ key <> ": " <> T.intercalate "," (Set.toList values)
      | (key, values) <- sortedConflicts
      ]

checkForInvalidIpas :: Set Text -> IO ()
checkForInvalidIpas ipas = do
  -- check that all IPAs only contain valid IPA characters
  let invalidChars =
        Set.unions
          [ Set.filter (not . isIPAChar) (Set.fromList (T.unpack ipa))
          | ipa <- Set.toList ipas
          ]
  unless (Set.null invalidChars) $
    throwIO . userError $
      "error: found invalid IPA characters that may cause lexurgy to fail: "
        <> T.unpack (show invalidChars)

-- |
-- Construct a mapping from spellings to their IPA transcriptions.
--
-- Given a list of groups, where each group is a list of (spelling, ipa) pairs
-- (e.g. multiple sources or datasets), this function merges all groups into a
-- single 'Map' from spelling to a 'Set' of IPA strings.
--
-- Behavior
-- - Each inner list is converted into a 'Map' mapping spellings to singleton
--   sets of IPA strings.
-- - The per-group maps are merged with 'unionsWith' and 'Set.union' so that all
--   IPA alternatives for a spelling are collected.
-- - Duplicate IPA entries for the same spelling are de-duplicated via 'Set'.
-- - If an individual group contains multiple entries with the same key, those
--   entries (and those from other groups) are all collected into the resulting
--   set for that key.
--
-- Example:
--   Given two groups:
--     [("cat","kæt"), ("dog","dɔg"), ("cat","kæt_alt")]
--     [("cat","kætʰ"), ("bird","bɜːd")]
--   The result will map:
--     "cat"  -> {"kæt", "kæt_alt", "kætʰ"}
--     "dog"  -> {"dɔg"}
--     "bird" -> {"bɜːd"}
--
-- Complexity:
--   Dominated by map and set insert/union costs; roughly O(n log m) where n is
--   the total number of pairs and m is the number of distinct spellings.
--
-- Notes:
-- - Uses 'Text' for spellings and IPA and returns a 'Map' and 'Set' to ensure
--   efficient lookups and uniqueness of IPA variants.
-- - The outer list preserves provenance before merging (each element is a group).
-- - Intended to merge multiple sources into a single spelling -> set-of-IPA map.
mkSpellingToIpa :: [[(Text, Text)]] -> IO (Map Text (Set Text))
mkSpellingToIpa list =
  if noEmptyIpas
  then pure $ Map.unionsWith Set.union $ toMap <$> list
  else errorIO errorMsg
  where
    toMap :: [(Text, Text)] -> Map Text (Set Text)
    toMap entries =
      Map.fromListWith
        Set.union
        [ (T.strip spelling, Set.singleton . normalizeIpaText $ T.strip ipa)
        | (spelling, ipa) <- entries
        ]
    noEmptyIpas = List.all (List.all (\ (_, ipa) -> not $ T.null ipa)) list
    errorMsg = T.unlines $ "Misparssed lines: " : misparssedLines
    misparssedLines = do
      sublist <- list
      (spelling, ipa) <- sublist
      guard (T.null ipa)
      pure spelling


-- | Handle fatal exceptions from the program.
--
-- This function is intended as a top-level exception handler. It performs three
-- actions when an exception is caught:
--
-- 1. Prints a brief error message including the exception's 'show' representation.
-- 2. Prints a usage hint ("Usage: Main <input-file> <output-directory>").
-- 3. Prints the current command-line arguments (the result of 'getArgs') to aid
--    debugging/troubleshooting.
--
-- Finally, it terminates the process with 'ExitFailure 1'.
--
-- Note: because the handler prints the exception using 'show', the message may
-- contain additional implementation-specific information. This handler should
-- be used at the top-level of the program (e.g. via 'catch' or an equivalent
-- global exception hook).
--
-- Arguments:
--   * The captured 'SomeException' to report and cause process termination.
--
-- Side effects:
--   * Writes to standard output and calls 'exitWith' to terminate the process.
--
-- Example usage:
--   main = myMain `catch` exceptionHandler
--
-- See also: consider more fine-grained handlers if different exceptions
-- should result in different exit codes or recovery strategies.
--
-- Stability: intended for use as a simple, user-facing error reporter.
exceptionHandler :: SomeException -> IO ()
exceptionHandler ex = do
  hPutStrLn stderr $ "An error occurred: " <> Text.pack (displayException ex)
  hPutStrLn stderr "Usage: Main <input-file> <output-directory>"
  hPutStrLn stderr . ("getArgs returned: " <>) . show =<< getArgs

  exitWith (ExitFailure 1)

-- | Read a UTF-8 encoded file and return its contents as Text.
-- IO exceptions are annotated with the file path for better diagnostics.
readUtf8File :: FilePath -> IO Text
readUtf8File path =
  -- annotate IO errors with the path to help trace failures
  annotateIO ("reading file " <> T.pack path) (decodeUtf8 <$> BS.readFile path)

-- | Print any empty keys or empty values present in a map (diagnostic).
reportEmptyEntries :: Text -> Map.Map Text (Set.Set Text) -> IO ()
reportEmptyEntries name mp = do
  let keysWithEmptyText = Map.keys $ Map.filter (Set.member "") mp
      keysWithEmptySets = Map.keys $ Map.filter Set.null mp
      valuesOfEmptyKeys = Map.lookup "" mp
      (errorMsg1 :: Maybe Text) =
        if List.null keysWithEmptyText then Nothing
          else pure $ "keys to empty string: " <> show keysWithEmptyText
      errorMsg2 =
        if List.null keysWithEmptySets then Nothing
          else pure $ "keys to empty sets: " <> show keysWithEmptySets
      errorMsg3 = ("the empty key leads to: " <>) . show <$> valuesOfEmptyKeys
      errorMsg = T.unlines . filterNothings $ [errorMsg1, errorMsg2, errorMsg3]
  unless (T.null errorMsg) $ do
    writeFile "keysWIthEmptyText.log" (encodeUtf8 $ T.unlines keysWithEmptyText)
    writeFile "keysWithEmptySets.log" (encodeUtf8 $ T.unlines keysWithEmptySets)
    writeFile "valuesOfEmptyKeys.log" (maybe "NOTHING" (encodeUtf8 . T.unlines . Set.toList) valuesOfEmptyKeys)
    errorIO $ "Error: " <> name <> "\n" <> errorMsg
  where
    filterNothings :: [Maybe a] -> [a]
    filterNothings = List.foldr f []
    f :: Maybe a -> [a] -> [a]
    f (Just x) list = x : list
    f Nothing list = list

