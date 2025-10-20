{-# LANGUAGE OverloadedStrings #-}

module MyLib.TestIPANormalize (tests) where

import Prelude (String, ($), not)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import Data.Text qualified as T

import MyLib.IPANormalize (normalizeIpaText)

tests :: TestTree
tests = testGroup "IPANormalize"
  [ testCase "normalizeIpaText maps presentation variants to canonical IPA" $
      let before = T.pack "I Ɪ ɪ a"
          after = normalizeIpaText before
      in do
        assertBool "contains canonical ɪ" ("ɪ" `T.isInfixOf` after)
        assertBool "does not contain ASCII I" (not ("I" `T.isInfixOf` after))
        assertBool "does not contain Ɪ variant" (not ("Ɪ" `T.isInfixOf` after))

  , QC.testProperty "normalizeIpaText is idempotent" $
      \(s :: String) ->
        let t = T.pack s
        in normalizeIpaText (normalizeIpaText t) === normalizeIpaText t
  ]