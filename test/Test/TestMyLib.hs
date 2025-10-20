{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module TestMyLib (TestMyLib.tests) where

import Prelude (IO)
import Test.Tasty
import Test.Tasty.HUnit

import MyLib.TestEntryParser qualified as TestEntryParser
import MyLib.TestIPANormalize qualified as TestIPANormalize
import MyLib.TestLexurgyExport qualified as TestLexurgyExport
import MyLib.TestPrintToLexurgy qualified as TestPrintToLexurgy

tests :: TestTree
tests = testGroup "MyLib tests"
  [ TestEntryParser.tests
  , TestIPANormalize.tests
  , testGroup "Lexurgy Modules tests"
      [ TestLexurgyExport.tests
      , TestPrintToLexurgy.tests
      ]
  ]