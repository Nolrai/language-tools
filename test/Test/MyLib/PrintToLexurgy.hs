{-# LANGUAGE OverloadedStrings #-}

module MyLib.TestPrintToLexurgy (tests) where

import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "MyLib.TestPrintToLexurgy"
  [ testCase "placeholder: print to lexurgy" $
      assertBool "placeholder" True
  ]