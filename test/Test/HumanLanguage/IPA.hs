{-# LANGUAGE OverloadedStrings #-}

module Test.HumanLanguage.IPA (tests) where

import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "HumanLanguage.IPA"
    [ testCase "placeholder: IPA utilities" $
        assertBool "placeholder" True
    ]
