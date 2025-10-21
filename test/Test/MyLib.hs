{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.MyLib (tests) where
import Test.Tasty

import Test.MyLib.EntryParser qualified as EntryParser
import Test.MyLib.IPANormalize qualified as IPANormalize
import Test.MyLib.LexurgyExport qualified as LexurgyExport
import Test.MyLib.LexurgyInstances qualified as LexurgyInstances
import Test.MyLib.PrintToLexurgy qualified as PrintToLexurgy

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
