module Test.MyLib.LexurgyExport (tests) where

import Prelude
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "LexurgyExport"
  [ testCase "placeholder: lexurgy export" $
      assertBool "placeholder" True
  ]