-- | Small utilities for working with maps from keys to sets of values.
-- |
-- | This module provides lightweight helpers for building and manipulating
-- | relations stored as `Map` from `a` to `Set b` (a multimap / relation).
-- |
-- | Conventions:
-- |  * The type alias (a :=> b) denotes a map from keys of type `a` to a set
-- |    of values of type `b`.
-- |  * All functions preserve set semantics for values (duplicates are folded
-- |    into sets).
module Data.Dictionary.Utils
  ( (:=>),
    Data.Dictionary.Utils.fromList,
    reverseDict,
    composeDicts,
  )
where

import Data.Map.Strict as Map
import Data.Set as Set

-- | Type alias for a map from keys of type @a@ to a set of values of type @b@.
-- |
-- | Pronounced "a maps-to b". Useful shorthand for relation-like maps.
type a :=> b = Map a (Set b)

-- | Build a relation (map-of-sets) from a list of (key, value) pairs.
-- |
-- | Duplicate keys in the input list will have their values collected into a
-- | 'Set' via 'Set.union'. Complexity is O(n * log m) where m is number of
-- | distinct keys (map/set insertion costs).
--
-- Example:
-- >>> fromList [('a',1),('a',2),('b',3)]
-- fromList [("a",fromList [1,2]),("b",fromList [3])]
fromList :: (Ord a, Ord b) => [(a, b)] -> a :=> b
fromList entries =
  fromListWith
    Set.union
    [ (key, Set.singleton value)
    | (key, value) <- entries
    ]

-- | Invert a relation: turn a -> {b1,b2,...} into b -> {a1,a2,...}.
-- |
-- | Every (key, value) pair in the input map becomes (value, key) in the
-- | resulting map; if multiple keys map to the same value, the keys are
-- | collected into a set.
--
-- Complexity: proportional to the total number of (key, value) pairs.
reverseDict :: (Ord a, Ord b) => a :=> b -> b :=> a
reverseDict dict =
  Map.fromListWith
    Set.union
    [ (value, Set.singleton key)
    | (key, values) <- Map.toList dict,
      value <- Set.toList values
    ]

-- | Compose two relations (maps-of-sets).
-- |
-- | Given map1 : a -> {b} and map2 : b -> {c}, produce map : a -> {c} such
-- | that there is an edge a -> c iff there exists b with a -> b in map1 and
-- | b -> c in map2. Intermediate keys with no mapping in map2 are ignored.
--
-- Example:
-- >>> composeDicts (fromList [('x','y')]) (fromList [('y','z')])
-- fromList [('x',fromList ['z'])]
--
-- Complexity: proportional to the number of (a,b) pairs plus the cost of
-- looking up each b in map2 (map lookup cost).
composeDicts :: (Ord a, Ord b, Ord c) => a :=> b -> b :=> c -> a :=> c
composeDicts map1 map2 =
  Map.fromListWith
    Set.union
    [ (key1, values2)
    | (key1, values1) <- Map.toList map1,
      value1 <- Set.toList values1,
      let maybeValues2 = Map.lookup value1 map2,
      Just values2 <- [maybeValues2]
    ]
