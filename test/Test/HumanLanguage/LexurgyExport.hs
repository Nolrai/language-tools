module Test.HumanLanguage.LexurgyExport (tests) where

import Control.Monad (guard)
import Data.ByteString qualified as B
import Data.IntMap.Strict qualified as Map
import Data.IntSet qualified as IntSet
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import HumanLanguage.IPA (ipaChars)
import HumanLanguage.IPANormalize (normalizeIpaText)
import HumanLanguage.LexurgyExport (featuresMap, lexurgyPath, lexurgyPrelude)
import HumanLanguage.LexurgyTypes (LexurgyMeaning (..))
import System.Directory (removeFile)
import System.Exit (ExitCode (..))
import System.IO (IOMode (WriteMode), withFile)
import System.Process (readProcessWithExitCode)
import Test.Tasty
import Test.Tasty.HUnit
import Prelude

-- | The canonical set of IPA characters used in tests, normalized.
normalizedIpaChars :: T.Text
normalizedIpaChars = normalizeIpaText ipaChars

-- | Top-level test group exported to the test runner.
tests :: TestTree
tests =
  testGroup
    "LexurgyExport"
    [ testGroup
        "Lexurgy meanings vs IPA characters"
        [ testCase "Every and only valid IPA characters have Lexurgy meanings" $
            let ipaCharsSet = IntSet.fromList $ map fromEnum (T.unpack normalizedIpaChars)
                lexurgyCharsSet = Map.keysSet featuresMap
                inconsistencies = ipaCharsSet IntSet.\\ lexurgyCharsSet
             in assertBool
                  ("Inconsistent IPA chars vs Lexurgy meanings: " <> show inconsistencies)
                  (IntSet.null inconsistencies),
          testCase "featuresMap is injective on nonMeta meanings" $
            (sequence_ :: [IO ()] -> IO ()) $ do
              (x :: Int) <- Map.keys featuresMap
              y <- Map.keys featuresMap
              guard (x < y)
              let fx = Map.lookup x featuresMap
                  fy = Map.lookup y featuresMap
              guard (fx /= Just LexurgyMeta)
              return . flip assertBool (fx /= fy) $
                "featuresMap is not injective for chars: "
                  <> show (toEnum x :: Char)
                  <> " and "
                  <> show (toEnum y :: Char)
                  <> " with features "
                  <> show fx
                  <> " vs "
                  <> show fy
        ],
      testGroup
        "Lexurgy executable"
        [ testCase "We can find lexurgy" $ do
            runLexurgy ["-h"],
          withPreludeFile $ testCase "lexurgy parses the prelude" $ do
            contents <- B.readFile preludeFilePath
            let fileLength = B.length contents
            putStrLn $ "Prelude contents are :" <> show fileLength <> " bytes long."
            runLexurgy ["sc", preludeFilePath]
        ]
    ]

-- | Path used for the temporary prelude file written by tests.
preludeFilePath :: FilePath
preludeFilePath = "lexurgy_prelud.sc"

-- | Wrap a TestTree with setup/teardown that writes the lexurgy prelude to a file.
-- The resource writes the file before the test runs and removes it afterwards.
withPreludeFile :: TestTree -> TestTree
withPreludeFile = withResource mkFile cleanUp . const
  where
    -- \| Create the prelude file by writing the encoded lexurgy prelude.
    mkFile = withFile preludeFilePath WriteMode (\handle -> B.hPut handle (encodeUtf8 lexurgyPrelude))
    -- \| Remove the prelude file created for the test.
    cleanUp _ = removeFile preludeFilePath

-- | Run the lexurgy executable with the given arguments and fail the test on non-zero exit.
runLexurgy :: [String] -> IO ()
runLexurgy args = do
  (exitCode, stdout, stderr) <- readProcessWithExitCode lexurgyPath args ""
  case exitCode of
    ExitSuccess -> putStrLn stdout
    ExitFailure code -> do
      putStrLn $ "Lexurgy exited with code: " <> show code
      putStrLn stderr
      assertFailure $
        "Lexurgy execution failed with code: "
          <> show code
          <> "\n\tand stderr: \n"
          <> stderr
          <> "\n\tand stdout: \n"
          <> stdout
