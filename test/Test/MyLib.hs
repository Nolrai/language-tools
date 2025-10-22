{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.MyLib (tests) where
import Test.Tasty

import Test.HumanLanguage.EntryParser qualified as EntryParser
import Test.HumanLanguage.IPANormalize qualified as IPANormalize
import Test.HumanLanguage.LexurgyExport qualified as LexurgyExport
import Test.HumanLanguage.LexurgyInstances qualified as LexurgyInstances
import Test.HumanLanguage.PrintToLexurgy qualified as PrintToLexurgy

tests :: TestTree
tests = testGroup "MyLib tests"
  [ EntryParser.tests
  , IPANormalize.tests
  , testGroup "Lexurgy Modules tests"
      [ LexurgyInstances.tests
      , LexurgyExport.tests
      , PrintToLexurgy.tests
      ]
  ]
