{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module MyLib.TestLexurgyExport (tests) where

import Prelude
import Control.Exception (try, evaluate, SomeException)
import Control.Monad (forM_, when)
import Test.Tasty
import Test.Tasty.HUnit
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import MyLib.LexurgyExport

-- Replace manual lists with TH-generated ones
allVowelFeatures :: [VowelFeature]
allVowelFeatures = $(enumerateType ''VowelFeature)

allConsonantFeatures :: [ConsonantFeature]
allConsonantFeatures = $(enumerateType ''ConsonantFeature)

allFloatingFeatures :: [FloatingFeature]
allFloatingFeatures = $(enumerateType ''FloatingFeature)

allFeatures :: [Feature]
allFeatures = $(enumerateType ''Feature)

tests :: TestTree
tests =
  [ testGroup "LexurgyExport Enum instances"
      [ testEnum allVowelFeatures
      , testEnum allConsonantFeatures
      , testEnum allFloatingFeatures
      , testEnum allFeatures
      ]
  ]