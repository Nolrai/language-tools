{-# LANGUAGE OverloadedStrings #-}

module Test.Data.Dictionary.Utils (tests) where

import Prelude
import Test.Tasty
import Test.Tasty.HUnit

import Data.Dictionary.Utils
  ( (:=>)
  , fromList
  , reverseDict
  , composeDicts
  , linesToDict
  , dictToLines
  )

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import qualified Data.Text as T

-- | Top-level test group for Data.Dictionary.Utils
tests :: TestTree
tests = testGroup "Data.Dictionary.Utils"
  [ testCase "fromList collects values into sets" test_fromList
  , testCase "reverseDict inverts relation" test_reverseDict
  , testCase "composeDicts composes two relations" test_composeDicts
  , testCase "linesToDict parses lines with separator" test_linesToDict
  , testCase "linesToDict/dictToLines roundtrip" test_linesToDict_roundtrip
  ]

test_fromList :: Assertion
test_fromList =
  let m :: Text :=> Int
      m = fromList [("a", 1), ("a", 2), ("b", 3)]
      expected = Map.fromList [("a", Set.fromList [1,2]), ("b", Set.fromList [3])]
  in assertEqual "fromList should collect duplicate keys into sets" expected m

test_reverseDict :: Assertion
test_reverseDict =
  let m :: Text :=> Int
      m = fromList [("a", 1), ("b", 1), ("c", 2)]
      rev = reverseDict m
      expected = Map.fromList [(1, Set.fromList ["a","b"]), (2, Set.fromList ["c"])]
  in assertEqual "reverseDict should invert mapping" expected (Map.mapKeys id $ Map.map (Set.map id) rev)

test_composeDicts :: Assertion
test_composeDicts =
  let m1 :: Text :=> Text
      m1 = fromList [("x", "y")]
      m2 :: Text :=> Text
      m2 = fromList [("y", "z")]
      composed = composeDicts m1 m2
      expected = fromList [("x", "z")] :: Text :=> Text
  in assertEqual "composeDicts should follow intermediate mappings" expected composed

test_linesToDict :: Assertion
test_linesToDict = do
  let contents = T.unlines ["a => 1", "b => 2", "a => 3", ""]
  dict <- linesToDict "=>" contents
  let expected = Map.fromList [("a", Set.fromList ["1","3"]), ("b", Set.fromList ["2"])]
  assertEqual "linesToDict should parse separator and merge duplicates" expected dict

test_linesToDict_roundtrip :: Assertion
test_linesToDict_roundtrip = do
  let pairs = ["a => x","a => y","b => z"]
  dict <- linesToDict "=>" (T.unlines pairs)
  out <- dictToLines " => " dict
  dict' <- linesToDict "=>" out
  assertEqual "roundtrip" dict dict'