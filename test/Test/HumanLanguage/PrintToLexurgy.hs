{-# LANGUAGE OverloadedStrings #-}

module Test.HumanLanguage.PrintToLexurgy (tests) where

import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "HumanLanguage.PrintToLexurgy"
    [ testCase "placeholder: print to lexurgy" $
        assertBool "placeholder" True
    ]
