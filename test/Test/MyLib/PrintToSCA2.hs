{-# LANGUAGE OverloadedStrings #-}

module MyLib.TestPrintToSCA2 (tests) where

import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "MyLib.TestPrintToSCA2"
  [ testCase "placeholder: print to SCA2" $
      assertBool "placeholder" True
  ]