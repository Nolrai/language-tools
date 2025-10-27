{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-- | Generic enumeration helpers.
-- |
-- | The module provides:
-- |  * `Enumerable` — a Generic-based source of all values for an ADT (as a Vector).
-- |  * `makeEnumOps` — build efficient (toEnum, fromEnum, succ, pred) operations
-- |    from a precomputed Vector of values.
-- |
-- | Important semantics note:
-- |  - succ/pred produced by the current helpers are *saturating*: calling `succ`
-- |    on the last element returns the last element; calling `pred` on the first
-- |    element returns the first element. This is intentional and the default
-- |    behavior of `makeEnumOps` (so tests and call sites expect saturation).
-- |  - toEnum/fromEnum still perform bounds-checking and will error if toEnum is
-- |    called with an out-of-range index.
-- |
-- | If you prefer strict Enum semantics (succ/pred error at bounds) you can
-- | replace uses of `makeEnumOps` with a custom wrapper that uses `toEnum`
-- |/`fromEnum` for succ/pred.
module HumanLanguage.EnumerateGeneric
  ( Enumerable (..),
    GEnumerable (..),
    buildIndex,
    toEnumFromVec,
    fromEnumUsingMap,
    defaultSuccUsing,
    defaultPredUsing,
    makeEnumOps,
    minBoundFromVec,
    maxBoundFromVec,
  )
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Vector (Vector)
import Data.Vector qualified as V
import GHC.Generics
  ( Generic (Rep, to),
    K1 (K1),
    M1 (M1),
    U1 (..),
    V1,
    type (:*:) (..),
    type (:+:) (..),
  )
import Prelude

-- runtime enumerator (now returns a Vector)
class Enumerable a where
  allValues :: Vector a
  default allValues :: (Generic a, GEnumerable (Rep a)) => Vector a
  allValues = V.map to gAllValues

-- Generic representation -> vector of values
class GEnumerable f where
  gAllValues :: Vector (f x)

instance GEnumerable V1 where
  gAllValues = V.empty

instance GEnumerable U1 where
  gAllValues = V.singleton U1

instance (GEnumerable a, GEnumerable b) => GEnumerable (a :*: b) where
  gAllValues =
    let va = (gAllValues :: Vector (a x))
        vb = (gAllValues :: Vector (b x))
     in V.concatMap (\a' -> V.map (a' :*:) vb) va

instance (GEnumerable a, GEnumerable b) => GEnumerable (a :+: b) where
  gAllValues =
    let va = (gAllValues :: Vector (a x))
        vb = (gAllValues :: Vector (b x))
     in V.map L1 va V.++ V.map R1 vb

instance (GEnumerable f) => GEnumerable (M1 i c f) where
  gAllValues = V.map M1 gAllValues

-- For a plain field type we require Enum & Bounded (or provide an Enumerable instance separately)
instance {-# OVERLAPPABLE #-} (Enum c, Bounded c) => GEnumerable (K1 i c) where
  gAllValues = V.fromList $ map K1 (enumFromTo minBound maxBound)

-- Efficient index helpers -----------------------------------------------------

-- Build a Map from value -> index from a Vector (O(n) once)
buildIndex :: (Ord a) => Vector a -> Map a Int
buildIndex = V.ifoldl' (\m i a -> Map.insert a i m) Map.empty

-- O(1) index into the vector with bounds check
toEnumFromVec :: Vector a -> Int -> a
toEnumFromVec vec n
  | n < 0 || n >= V.length vec =
      error $ "toEnum: tag " ++ show n ++ " is outside of bounds"
  | otherwise = vec V.! n

-- O(log n) lookup via Map with pattern match (no linear search)
fromEnumUsingMap :: (Ord a, Show a) => Map a Int -> a -> Int
fromEnumUsingMap mp v = case Map.lookup v mp of
  Just i -> i
  Nothing -> error $ "fromEnum: invalid value " ++ show v

-- Succ/Pred using prebuilt structures (O(1) index + O(log n) lookup)
defaultSuccUsing :: (Ord a, Show a) => Vector a -> Map a Int -> a -> a
defaultSuccUsing vec mp x =
  let i = fromEnumUsingMap mp x
      lastIdx = V.length vec - 1
      nx = min (i + 1) lastIdx
   in vec V.! nx

defaultPredUsing :: (Ord a, Show a) => Vector a -> Map a Int -> a -> a
defaultPredUsing vec mp x =
  let i = fromEnumUsingMap mp x
      nx = if i <= 0 then 0 else i - 1
   in vec V.! nx

-- Convenience: build the index once and return the four functions.
-- Use this at module load to avoid recomputing.
makeEnumOps :: (Ord a, Show a) => Vector a -> (Int -> a, a -> Int, a -> a, a -> a)
makeEnumOps vec =
  let mp = buildIndex vec
      toE = toEnumFromVec vec
      fromE = fromEnumUsingMap mp
      succF = defaultSuccUsing vec mp
      predF = defaultPredUsing vec mp
   in (toE, fromE, succF, predF)

-- | Safe helpers to produce Bounded.minBound / Bounded.maxBound from a non-empty Vector.
minBoundFromVec :: Vector a -> a
minBoundFromVec vec
  | V.null vec = error "minBoundFromVec: empty enumeration"
  | otherwise = V.head vec

maxBoundFromVec :: Vector a -> a
maxBoundFromVec vec
  | V.null vec = error "maxBoundFromVec: empty enumeration"
  | otherwise = V.last vec
