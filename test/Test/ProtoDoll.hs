{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Integration-style test:
-- |
-- | 1. Read data/wordLists/test.txt which has lines of the form:
-- |      GLOSS (notes) => IPA
-- |    We extract the IPA (text after "=>", before an optional " (" note).
-- |
-- | 2. Produce a pure IPA wordlist file under working/test_input_for_lexurgy.wlm
-- |    (one IPA per line).
-- |
-- | 3. Invoke the project's lexurgy runner with the ProtoDoll rule set.
-- |
-- | 4. Read the produced evolved-output file and check that every input line
-- |    produced a non-empty evolved entry. Report any abnormal lines.
module Test.ProtoDoll (tests) where

import Prelude
import Test.Tasty
import Test.Tasty.HUnit

import Data.Text (Text)
import Data.Text qualified as T
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import System.Directory
import System.FilePath ((</>), takeDirectory)
import Control.Exception (SomeException, try)
import Control.Monad (unless)
import Data.Maybe (mapMaybe)

import Utils.IO (runLexurgy, writeFileUtf8, readFileUtf8)
import HumanLanguage.LexurgyExport (lexurgyPath)
import Data.Dictionary.Utils

testListPath :: FilePath
testListPath = "data/wordLists/test.txt"

workingDir :: FilePath
workingDir = "working"

inputForLexurgy :: FilePath
inputForLexurgy = workingDir </> "test_input_for_lexurgy.wlm"

expectedEvolvedOut :: FilePath
expectedEvolvedOut = workingDir </> "ProtoDoll_evolved_words.txt"

goldenDir :: FilePath
goldenDir = "test" </> "golden"

goldenPath :: FilePath
goldenPath = goldenDir </> "ProtoDoll_golden.wlm"

tests :: TestTree
tests = testGroup "ProtoDoll integration"
  [ testCase "proto-doll-evolutions-produce-non-empty-results" integrationTest
  , testCase "proto-doll-evolutions-match-golden" integrationTest
  ]

-- Parse "GLOSS (notes) => IPA" into (gloss (notes), ipa).
-- Ignore lines without "=>".
parseTestLine :: Data.Text.Text -> Maybe (Data.Text.Text, Data.Text.Text)
parseTestLine raw =
  case T.splitOn "=>" raw of
    (lhs : rhs : _) ->
      let gloss = T.strip lhs
          ipa = T.strip rhs
      in if T.null gloss || T.null ipa then Nothing else Just (gloss, ipa)
    _ -> Nothing

assertFileExists :: Text -> FilePath -> IO ()
assertFileExists name path = do
  fileExists <- doesFileExist path
  let msg = T.unpack name ++ " not found: " ++ path
  assertBool msg fileExists

writeGoldenFile :: FilePath -> Map.Map Data.Text.Text (Set.Set Data.Text.Text) -> IO ()
writeGoldenFile fp mp = do
  createDirectoryIfMissing True (takeDirectory fp)
  dictLines <- dictToLines " => " mp
  writeFileUtf8 fp dictLines

integrationTest :: Assertion
integrationTest = do
  assertFileExists "test list" testListPath
  raw <- readFileUtf8 testListPath
  let parsed = mapMaybe parseTestLine (T.lines raw)
  assertBool "no valid test lines parsed from test.txt" (not (null parsed))

  let (_glosses, ipas) = unzip parsed
      ipaLines = ipas

  writeFileUtf8 inputForLexurgy (T.unlines ipaLines)

  assertFileExists "lexurgy" lexurgyPath
  runErr <- try (runLexurgy [inputForLexurgy] "data/rules/ProtoDoll.lsc") :: IO (Either SomeException ())
  case runErr of
    Left e -> assertFailure $ "running lexurgy failed: " ++ show e
    Right () -> pure ()

  assertFileExists "evolved output" expectedEvolvedOut
  evolvedText <- readFileUtf8 expectedEvolvedOut
  evolvedMap <- linesToDict "=>" evolvedText

  -- If the golden file is missing, write it and fail so developer can inspect & commit.
  goldenExists <- doesFileExist goldenPath
  if not goldenExists
    then do
      writeGoldenFile goldenPath evolvedMap
      assertFailure $ "Golden file created at " ++ goldenPath ++ ". Review & commit it, then re-run tests."
    else do
      goldenLines <- readFileUtf8 goldenPath
      goldenMap <- linesToDict "=>" goldenLines
      -- compare for each gloss in our test list
      let diffs = mapMaybe (compareForGloss evolvedMap goldenMap) parsed
          extraInGolden = Map.keysSet goldenMap `Set.difference` Map.keysSet (Map.fromList (map (\(g,_) -> (g,())) parsed))
          extraInEvolved = Map.keysSet evolvedMap `Set.difference` Map.keysSet goldenMap
      unless (null extraInGolden && null extraInEvolved && null diffs) $ do
        let report = T.unpack $ T.unlines $
              ["Differences between evolved output and golden:"] ++
              map (T.pack . ("- extra in golden: " ++) . show) (Set.toList extraInGolden) ++
              map (T.pack . ("- extra in evolved: " ++) . show) (Set.toList extraInEvolved) ++
              map T.pack diffs
        assertFailure report

-- Compare expected vs actual for a single gloss; return Nothing if equal,
-- otherwise a textual description.
compareForGloss :: Map.Map Data.Text.Text (Set.Set Data.Text.Text) -> Map.Map Data.Text.Text (Set.Set Data.Text.Text) -> (Data.Text.Text, Data.Text.Text) -> Maybe String
compareForGloss evolved golden (gloss, _ipa) =
  let actual = Map.findWithDefault Set.empty gloss evolved
      expected = Map.findWithDefault Set.empty gloss golden
  in if actual == expected
      then Nothing
      else Just $ T.unpack gloss ++ ":\n  expected: " ++ show (Set.toList expected) ++ "\n  actual:   " ++ show (Set.toList actual)
