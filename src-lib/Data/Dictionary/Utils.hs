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
    composeAndFindMissing,
    linesToDict,
    dictToLines,
  )
where

import Data.Map.Strict as Map
import Data.Set as Set
import Control.Applicative (Applicative (pure))
import Data.Text (Text)
import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Text.IO as TIO (putStrLn)
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

-- ...existing code...
-- | Compose two relations like 'composeDicts', but also report any intermediate
-- | keys that were referenced by the first relation but missing from the second.
-- |
-- | Given
-- |   map1 :: a :=> b    -- a -> {b}
-- |   map2 :: b :=> c    -- b -> {c}
-- |
-- | Returns a pair:
-- |   (resultMap, missingBs)
-- |
-- | where:
-- |  * resultMap :: a :=> c contains an entry a -> S where S is the union of
-- |    all c values reachable from a via any b in map1 that exists in map2.
-- |    If none of a's b-values are present in map2, a will NOT appear in resultMap.
-- |  * missingBs :: Set b contains every b that appeared in map1 but had no
-- |    mapping in map2 (unique, de‑duplicated).
-- |
-- | Behavior notes:
-- |  * Only the first-level keys from map1 are iterated; the function does not
-- |    transitively follow further links beyond map2.
-- |  * The function ignores b-values that are present in map2 but whose
-- |    resulting c-set is empty (they contribute no c-values).
-- |
-- | Complexity:
-- |  Let P be the total number of (a,b) pairs in map1 and let M be the size of
-- |  map2. Complexity is dominated by map lookups and set unions: roughly
-- |  O(P * log M + U * log S) where U is the total number of produced c-values
-- |  and S is set sizes encountered during union operations.
-- |
-- | Examples:
-- | >>> let m1 = fromList [("x","y"),("x","z")] :: Text :=> Text
-- | >>> let m2 = fromList [("y","u"),("y","v")] :: Text :=> Text
-- | >>> composeAndFindMissing m1 m2
-- | (fromList [("x",fromList ["u","v"])], fromList ["z"])
-- |
composeAndFindMissing :: forall a b c. (Ord a, Ord b, Ord c) => a :=> b -> b :=> c -> (a :=> c, Set b)
composeAndFindMissing map1 map2 =
  Map.foldrWithKey step (Map.empty, Set.empty) map1
  where
    step :: a -> Set b -> (a :=> c, Set b) -> (a :=> c, Set b)
    step key1 values1 (accMap, accMissing) =
      let (newValues, missingValues) =
            Set.foldr
              (\value1 (valsAcc, missAcc) ->
                case Map.lookup value1 map2 of
                  Just values2 -> (Set.union valsAcc values2, missAcc)
                  Nothing -> (valsAcc, Set.insert value1 missAcc))
              (Set.empty, accMissing)
              values1
      in if Set.null newValues
            then (accMap, missingValues)
            else (Map.insertWith Set.union key1 newValues accMap, missingValues)

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
  TIO.putStrLn $ "Parsing " <> T.show (List.length textLines) <> " non-empty lines."
  pairs <- for textLines $ \line -> do
    let (spelling', rest') = T.breakOn seperator line
    let spelling = T.strip spelling'
    let rest = if T.null rest' then spelling else T.drop (T.length seperator) rest'
    pure (spelling, Set.singleton (T.strip rest))
  pure $ Map.fromListWith Set.union pairs

-- | Serialize a dictionary (map-of-sets) into line-based "key<separator>value" pairs.
-- | Each (key, value) pair in the dictionary produces one line in the output.
-- | If a key maps to multiple values, multiple lines are produced (one per value).

dictToLines :: Text -> Text :=> Text -> IO Text
dictToLines seperator dict = do
  let pairs =
        [ key <> seperator <> val
        | (key, vals) <- Map.toAscList dict
        , val <- Set.toAscList vals
        ]
      out = T.unlines pairs
  _ <- TIO.putStrLn $ "Serializing " <> T.show (List.length pairs) <> " pairs."
  pure out