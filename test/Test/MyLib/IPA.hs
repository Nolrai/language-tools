{-# LANGUAGE OverloadedStrings #-}

module Test.MyLib.IPA (tests) where

import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "MyLib.IPA"
  [ testCase "placeholder: IPA utilities" $
      assertBool "placeholder" True
  ]
