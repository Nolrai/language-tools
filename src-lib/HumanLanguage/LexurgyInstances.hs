{-# OPTIONS_GHC -Wno-orphans #-}

module HumanLanguage.LexurgyInstances () where

import HumanLanguage.EnumerateGeneric
  ( Enumerable (..),
    makeEnumOps,
    maxBoundFromVec,
    minBoundFromVec,
  )
import HumanLanguage.LexurgyTypes
import Prelude

-- VowelFeature helpers -------------------------------------------------------
instance Enumerable VowelFeature

toVowel :: Int -> VowelFeature
fromVowel :: VowelFeature -> Int
succVowel :: VowelFeature -> VowelFeature
predVowel :: VowelFeature -> VowelFeature
(toVowel, fromVowel, succVowel, predVowel) = makeEnumOps allValues

instance Bounded VowelFeature where
  minBound = minBoundFromVec allValues
  maxBound = maxBoundFromVec allValues

instance Enum VowelFeature where
  toEnum = toVowel
  fromEnum = fromVowel
  succ = succVowel
  pred = predVowel

-- ConsonantFeature helpers --------------------------------------------------
instance Enumerable ConsonantFeature

toConsonant :: Int -> ConsonantFeature
fromConsonant :: ConsonantFeature -> Int
succConsonant :: ConsonantFeature -> ConsonantFeature
predConsonant :: ConsonantFeature -> ConsonantFeature
(toConsonant, fromConsonant, succConsonant, predConsonant) = makeEnumOps allValues

instance Bounded ConsonantFeature where
  minBound :: ConsonantFeature
  minBound = minBoundFromVec allValues
  maxBound = maxBoundFromVec allValues

instance Enum ConsonantFeature where
  toEnum = toConsonant
  fromEnum = fromConsonant
  succ = succConsonant
  pred = predConsonant

-- FloatingFeature helpers ---------------------------------------------------
instance Enumerable FloatingFeature

toFloating :: Int -> FloatingFeature
fromFloating :: FloatingFeature -> Int
succFloating :: FloatingFeature -> FloatingFeature
predFloating :: FloatingFeature -> FloatingFeature
(toFloating, fromFloating, succFloating, predFloating) = makeEnumOps allValues

instance Bounded FloatingFeature where
  minBound = minBoundFromVec allValues
  maxBound = maxBoundFromVec allValues

instance Enum FloatingFeature where
  toEnum = toFloating
  fromEnum = fromFloating
  succ = succFloating
  pred = predFloating

-- Feature helpers -----------------------------------------------------------
instance Enumerable Feature

predFeature :: Feature -> Feature
succFeature :: Feature -> Feature
fromFeature :: Feature -> Int
toFeature :: Int -> Feature
(toFeature, fromFeature, succFeature, predFeature) = makeEnumOps allValues

instance Bounded Feature where
  minBound = minBoundFromVec allValues
  maxBound = maxBoundFromVec allValues

instance Enum Feature where
  toEnum = toFeature
  fromEnum = fromFeature
  succ = succFeature
  pred = predFeature
