{-# LANGUAGE OverloadedStrings, TypeApplications, AllowAmbiguousTypes #-}

module Test.HumanLanguage.LexurgyInstances (tests) where

import Prelude
import Control.Exception (try, evaluate, SomeException)
import Control.Monad (forM_)
import Test.Tasty
import Test.Tasty.HUnit
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import HumanLanguage.EnumerateGeneric

import HumanLanguage.LexurgyInstances()
import HumanLanguage.LexurgyTypes
  ( Feature
  , VowelFeature
  , ConsonantFeature
  , FloatingFeature
  )

tests :: TestTree
tests =
  testGroup "Lexurgy Enum instances"
      [ testGeneratedInstances @Feature
      , testGeneratedInstances @VowelFeature
      , testGeneratedInstances @ConsonantFeature
      , testGeneratedInstances @FloatingFeature
      ]

testGeneratedInstances :: forall a. (Enum a, Enumerable a, Show a, Bounded a, Ord a) => TestTree
testGeneratedInstances =
  testGroup "Generated Enum and Bounded instances"
    [ testEnum @a
    , testEnumMatchesEnumerable @a
    , testEnumeratedBounds @a
    , testEnumMatchesOrd @a
    ]

testEnum :: forall a. (Eq a, Enum a, Enumerable a, Show a) => TestTree
testEnum =
  let vec = allValues :: Vector a
      n = V.length vec in
  testGroup "Enum"
  [ testCase "succ"
      $ forM_ [0 .. n - 1] $ \i ->
          if i < n - 1
          then succ (vec ! i) @?= (vec ! (i + 1))
          else succ (vec ! (n - 1)) @?= (vec ! (n - 1))
  , testCase "pred"
      $ forM_ [0 .. n - 1] $ \i ->
          if i > 0
          then pred (vec ! i) @?= (vec ! (i - 1))
          else pred (vec ! 0) @?= (vec ! 0)
  , testCase "toEnum out of bounds (negative)" $ do
    err1 <- try (evaluate (toEnum (-1) :: a)) :: IO (Either SomeException a)
    case err1 of
      Left _  -> return ()
      Right _ -> assertFailure "Expected exception for toEnum (-1)"
  , testCase "toEnum out of bounds (too large)" $ do
    err2 <- try (evaluate (toEnum n :: a)) :: IO (Either SomeException a)
    case err2 of
      Left _  -> return ()
      Right _ -> assertFailure ("Expected exception for toEnum " ++ show n)
  ]

testEnumMatchesEnumerable :: forall a. (Eq a, Enum a, Enumerable a, Show a) => TestTree
testEnumMatchesEnumerable =
  let vec = allValues :: Vector a
      n = V.length vec in
  testGroup "Enum matches Enumerable"
  [ testCase "Enum instance matches Enumerable" $ do
    forM_ [0 .. n - 1] $ \i -> do
      let v = vec ! i
      -- fromEnum
      fromEnum v @?= i
      -- toEnum
      toEnum i @?= v
  ]

testEnumeratedBounds :: forall a. (Eq a, Enumerable a, Bounded a, Show a) => TestTree
testEnumeratedBounds =
  let vec = allValues :: Vector a
      n = V.length vec in
  testGroup "Bounded"
  [ testCase "minBound" $
      minBound @?= (vec ! 0)
  , testCase "maxBound" $
      maxBound @?= (vec ! (n - 1))
  ]

testEnumMatchesOrd :: forall a. (Ord a, Enumerable a)
  => TestTree
testEnumMatchesOrd =
  testCase "Enum and Ord instances agree" $ do
    let vec = allValues :: Vector a
        n = V.length vec
    forM_ [0 .. n - 1] $ \i -> do
      let v = vec ! i
      forM_ [0 .. n - 1] $ \j -> do
        let w = vec ! j
        compare v w @?= compare i j