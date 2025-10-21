module MyLib.LexurgyInstances
  ( allVowelFeatures
  , allConsonantFeatures
  , allFloatingFeatures
  , allFeatures
  ) where

import Prelude
import Data.Vector (Vector)
import MyLib.EnumerateGeneric
  ( Enumerable(..)
  , makeEnumOps
  , minBoundFromVec
  , maxBoundFromVec
  )
import MyLib.LexurgyTypes

-- VowelFeature helpers -------------------------------------------------------
instance Enumerable VowelFeature

allVowelFeatures :: Vector VowelFeature
allVowelFeatures = allValues

(toVowel, fromVowel, succVowel, predVowel) = makeEnumOps allVowelFeatures

instance Bounded VowelFeature where
  minBound = minBoundFromVec allVowelFeatures
  maxBound = maxBoundFromVec allVowelFeatures

instance Enum VowelFeature where
  toEnum = toVowel
  fromEnum = fromVowel
  succ = succVowel
  pred = predVowel

-- ConsonantFeature helpers --------------------------------------------------
instance Enumerable ConsonantFeature

allConsonantFeatures :: Vector ConsonantFeature
allConsonantFeatures = allValues

(toConsonant, fromConsonant, succConsonant, predConsonant) = makeEnumOps allConsonantFeatures

instance Bounded ConsonantFeature where
  minBound = minBoundFromVec allConsonantFeatures
  maxBound = maxBoundFromVec allConsonantFeatures

instance Enum ConsonantFeature where
  toEnum = toConsonant
  fromEnum = fromConsonant
  succ = succConsonant
  pred = predConsonant

-- FloatingFeature helpers ---------------------------------------------------
instance Enumerable FloatingFeature

allFloatingFeatures :: Vector FloatingFeature
allFloatingFeatures = allValues

(toFloating, fromFloating, succFloating, predFloating) = makeEnumOps allFloatingFeatures

instance Bounded FloatingFeature where
  minBound = minBoundFromVec allFloatingFeatures
  maxBound = maxBoundFromVec allFloatingFeatures

instance Enum FloatingFeature where
  toEnum = toFloating
  fromEnum = fromFloating
  succ = succFloating
  pred = predFloating

-- Feature helpers -----------------------------------------------------------
instance Enumerable Feature

allFeatures :: Vector Feature
allFeatures = allValues

(toFeature, fromFeature, succFeature, predFeature) = makeEnumOps allFeatures

instance Bounded Feature where
  minBound = minBoundFromVec allFeatures
  maxBound = maxBoundFromVec allFeatures

instance Enum Feature where
  toEnum = toFeature
  fromEnum = fromFeature
  succ = succFeature
  pred = predFeature