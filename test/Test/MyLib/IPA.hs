{-# LANGUAGE OverloadedStrings #-}

module MyLib.TestIPA (tests) where

import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "MyLib.TestIPA"
  [ testCase "placeholder: IPA utilities" $
      assertBool "placeholder" True
  ]