{-# LANGUAGE OverloadedStrings #-}

module Test.MyLib.PrintToSCA2 (tests) where

import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "MyLib.PrintToSCA2"
  [ testCase "placeholder: print to SCA2" $
      assertBool "placeholder" True
  ]
