{-# LANGUAGE OverloadedStrings #-}

module Test.HumanLanguage.PrintToSCA2 (tests) where

import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "HumanLanguage.PrintToSCA2"
    [ testCase "placeholder: print to SCA2" $
        assertBool "placeholder" True
    ]
