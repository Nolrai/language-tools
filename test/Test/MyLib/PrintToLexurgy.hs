{-# LANGUAGE OverloadedStrings #-}

module Test.MyLib.PrintToLexurgy (tests) where

import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "MyLib.PrintToLexurgy"
  [ testCase "placeholder: print to lexurgy" $
      assertBool "placeholder" True
  ]
