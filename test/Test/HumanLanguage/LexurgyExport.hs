module Test.HumanLanguage.LexurgyExport (tests) where

import Prelude
import Test.Tasty
import Test.Tasty.HUnit
import Data.IntSet qualified as Set
import Data.IntMap.Strict qualified as Map
import Data.ByteString qualified as B
import Data.Text.Encoding (encodeUtf8)
import System.Process (readProcessWithExitCode)

import HumanLanguage.LexurgyExport ( featuresMap, lexurgyPath, lexurgyPrelude )
import HumanLanguage.IPA (ipaChars)
import System.IO (withFile, IOMode (WriteMode))
import System.Directory (removeFile)
import System.Exit (ExitCode(..))
import qualified Data.Text as T
import HumanLanguage.IPANormalize (normalizeIpaText)

normalizedIpaChars :: T.Text
normalizedIpaChars = normalizeIpaText ipaChars

tests :: TestTree
tests = testGroup "LexurgyExport"
  [ testCase "Every and only valid IPA characters have Lexurgy meanings" $
      let ipaCharsSet = Set.fromList $ map fromEnum (T.unpack normalizedIpaChars)
          lexurgyCharsSet = Map.keysSet featuresMap
          inconsistencies = ipaCharsSet Set.\\ lexurgyCharsSet
      in  assertBool ("Inconsistent IPA chars vs Lexurgy meanings: " <> show inconsistencies)
            (Set.null inconsistencies)

  , testCase "We can find lexurgy" $ do
    runLexurgy ["-h"]

  , withPreludeFile $ testCase "lexurgy parses the prelude" $ do
      contents <- B.readFile preludeFilePath
      let fileLength = B.length contents
      putStrLn $ "Prelude contents are :" <> show fileLength <> " bytes long."
      runLexurgy ["sc", preludeFilePath]

  ]

preludeFilePath :: FilePath
preludeFilePath = "lexurgy_prelud.sc"

withPreludeFile :: TestTree -> TestTree
withPreludeFile = withResource mkFile cleanUp . const
  where
    mkFile = withFile preludeFilePath WriteMode (\ handle -> B.hPut handle (encodeUtf8 lexurgyPrelude))
    cleanUp _ = removeFile preludeFilePath

runLexurgy :: [String] -> IO ()
runLexurgy args = do
  (exitCode, stdout, stderr) <- readProcessWithExitCode lexurgyPath args ""
  case exitCode of
    ExitSuccess -> putStrLn stdout
    ExitFailure code -> do
      putStrLn $ "Lexurgy exited with code: " <> show code
      putStrLn stderr
      assertFailure
        $ "Lexurgy execution failed with code: " <> show code
        <> "\n\tand stderr: \n" <> stderr
        <> "\n\tand stdout: \n" <> stdout