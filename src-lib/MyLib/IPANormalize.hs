module MyLib.IPANormalize
  ( presentationVariants
  , normalizeIpaChar
  , normalizeIpaText
  ) where

import Prelude
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap)
import Data.Char (ord, chr)
import qualified Data.Map.Strict as Map

-- Definitive mapping of presentation / legacy variants -> canonical IPA char.
-- Keys are characters that users commonly paste or type; values are the canonical
-- IPA codepoints used in the rest of the codebase (features maps, printing, etc).
--
-- Update this list if you accept more legacy inputs.
presentationVariants :: [(Char, Char)]
presentationVariants =
  [ ('I',  'ɪ')   -- U+0049 LATIN CAPITAL LETTER I  -> U+026A LATIN SMALL LETTER CAPITAL I (IPA ɪ)
  , ('Ɪ',  'ɪ')   -- U+A78E LATIN CAPITAL LETTER SMALL CAPITAL I -> U+026A ɪ
  -- add other mappings here as needed, e.g.
  -- , ('ASCII-ALT', 'canonical-ipa')
  ]

variantMap :: Map.Map Char Char
variantMap = Map.fromList presentationVariants

-- Normalize a single character: map presentation variants to canonical IPA.
-- Characters not in the table are returned unchanged.
normalizeIpaChar :: Char -> Char
normalizeIpaChar c = Map.findWithDefault c c variantMap

-- Normalize a whole Text value by mapping each character.
-- Use this on raw IPA input before tokenization or feature lookup.
normalizeIpaText :: Text -> Text
normalizeIpaText = T.map normalizeIpaChar
