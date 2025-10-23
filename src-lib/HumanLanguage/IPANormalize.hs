{-# LANGUAGE OverloadedStrings #-}
module HumanLanguage.IPANormalize
  ( presentationVariants
  , normalizeIpaChar
  , normalizeIpaText
  ) where

import Prelude
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map

-- Definitive mapping of presentation / legacy variants -> canonical IPA char.
-- Keys are characters that users commonly paste or type; values are the canonical
-- IPA codepoints used in the rest of the codebase (features maps, printing, etc).
--
-- Update this list if you accept more legacy inputs.
presentationVariants :: [(Char, Text)]
presentationVariants =
  [ ('I',  "ɪ")   -- U+0049 LATIN CAPITAL LETTER I  -> U+026A LATIN SMALL LETTER CAPITAL I (IPA ɪ)
  , ('Ɪ',  "ɪ")   -- U+A78E LATIN CAPITAL LETTER SMALL CAPITAL I -> U+026A ɪ
  , ('\865', "") -- U+0361 COMBINING DOUBLE INVERTED BREVE (tie bar) is ignored
  -- strip tone letters
  , ('\742', "") -- U+02E6 MODIFIER LETTER EXTRA-HIGH TONE BAR
  , ('\743', "") -- U+02E7 MODIFIER LETTER HIGH TONE BAR
  , ('\744', "") -- U+02E8 MODIFIER LETTER MID TONE BAR
  , ('\745', "") -- U+02E9 MODIFIER LETTER LOW TONE BAR
  , ('\746', "") -- U+02EA MODIFIER LETTER EXTRA-LOW TONE BAR
  -- normalize length marks to U+02D0 MODIFIER LETTER TRIANGULAR COLON
  , ('\720',  "\720") -- U+02D0 MODIFIER LETTER TRIANGULAR COLON
  , ('\721',  "\720") -- U+02D1 MODIFIER LETTER HALF TRIANGULAR COLON
  -- Normalize ring diacritics to the combining ring below U+0310
  , ('\778', "\805") -- U+030A COMBINING RING ABOVE
  , ('\805', "\805") -- U+0310 COMBINING RING BELOW
  , ('\858', "\805") -- U+030B COMBINING DOUBLE RING BELOW
  -- add other mappings here as needed, e.g.
  ]

variantMap :: Map.Map Char Text
variantMap = Map.fromList presentationVariants

-- Normalize a single character: map presentation variants to canonical IPA.
-- Characters not in the table are returned unchanged.
normalizeIpaChar :: Char -> Text
normalizeIpaChar c = Map.findWithDefault (T.singleton c) c variantMap

-- Normalize a whole Text value by mapping each character.
-- Use this on raw IPA input before tokenization or feature lookup.
normalizeIpaText :: Text -> Text
normalizeIpaText = T.concatMap normalizeIpaChar
