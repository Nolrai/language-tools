module Main (main) where

import Test.HumanLanguage.EntryParser qualified as EntryParser
import Test.HumanLanguage.IPANormalize qualified as IPANormalize
import Test.HumanLanguage.LexurgyExport qualified as LexurgyExport
import Test.HumanLanguage.LexurgyInstances qualified as LexurgyInstances
import Test.HumanLanguage.PrintToLexurgy qualified as PrintToLexurgy
import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
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
