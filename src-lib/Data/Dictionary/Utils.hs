{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

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
    linesToDict,
  )
where

import Data.Map.Strict as Map
import Data.Set as Set
import Control.Applicative (Applicative (pure))
import Data.Text (Text)
import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Ord
import Data.Maybe
import System.IO (IO)
import Data.Traversable (for)
import Data.Function (($))
import Data.Bool
import Control.Category
import Data.Monoid

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

{-|
Parse a block of text containing line-based "spelling<separator>ipa" pairs into a
dictionary mapping each spelling to a set of IPA transcriptions.

Behavior
- The input `contents` is split on newline characters; empty lines are ignored.
- Each non-empty line is split at the first occurrence of the given `seperator`
  (note: `seperator` may be a multi-character Text). The part before the first
  occurrence is treated as the spelling/key; the part after the separator is the
  IPA value.
- The IPA value and spelling are trimmed of leading/trailing whitespace.
- If a line does not contain the separator, the function signals a failure using
  `fail` from `MonadFail m`. The failure message looks like:
  "Invalid line (missing separator '<seperator>'): <line>"
- If multiple lines yield the same spelling/key, their IPA sets are merged using
  set union; the final result is a mapping from spelling to a set of IPA texts.

Type and error handling
- The function runs in any monad `m` satisfying `MonadFail m` and `Alternative m`.
  Parsing errors are reported via `fail`, so callers should use an appropriate
  monad (e.g. `Either String`, `Maybe`, or `IO`) depending on desired error
  semantics.
- The result type is `m (Text :=> Text)`, where `(:=>)` denotes the map/dictionary
  type used in the code and values are `Set Text`.

Notes
- `Text.breakOn` is used to locate the separator, so only the first occurrence
  of the separator on a line is considered; any subsequent separators remain as
  part of the IPA text.
- Complexity is linear in the size of `contents` (processing each line once).
- Example:
    Given separator ":" and contents:
      "word: wɜːd\nfoo: fʊ\nword: wɜːrd"
    the resulting map contains "word" -> {"wɜːd", "wɜːrd"} and "foo" -> {"fʊ"}.
-}
linesToDict :: Text -> Text -> IO (Text :=> Text)
linesToDict seperator contents = do
  let textLines = List.filter (not . T.null) (T.lines contents)
  _ <- TIO.putStrLn $ "Parsing " <> T.show (List.length textLines) <> " non-empty lines."
  pairs <- for textLines $ \line -> do
    let (spelling', rest') = T.breakOn seperator line
    let spelling = T.strip spelling'
    let rest = if T.null rest' then spelling else rest'
    let ipa = T.strip (T.drop (T.length seperator) rest)
    pure (spelling, Set.singleton ipa)
  pure $ Map.fromListWith Set.union pairs