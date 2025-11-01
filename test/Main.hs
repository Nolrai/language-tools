module Main (main) where

import Test.HumanLanguage.EntryParser qualified as EntryParser
import Test.HumanLanguage.IPANormalize qualified as IPANormalize
import Test.HumanLanguage.LexurgyExport qualified as LexurgyExport
import Test.HumanLanguage.LexurgyInstances qualified as LexurgyInstances
import Test.HumanLanguage.PrintToLexurgy qualified as PrintToLexurgy
import Test.Tasty
import Test.Data.Dictionary.Utils qualified as DictionaryUtils
import Test.IOUtf8

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "All tests"
  [ testGroup
    "HumanLanguage tests"
    [ EntryParser.tests,
      IPANormalize.tests,
      testGroup
        "Lexurgy Modules tests"
        [ LexurgyInstances.tests,
          LexurgyExport.tests,
          PrintToLexurgy.tests
        ]
    ]
  , testGroup "Utils tests"
    [ DictionaryUtils.tests,
      Test.IOUtf8.tests
    ]
  ]