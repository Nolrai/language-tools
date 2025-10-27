{-# LANGUAGE OverloadedStrings #-}

module HumanLanguage.IPANormalize
  ( presentationVariants,
    normalizeIpaChar,
    normalizeIpaText,
  )
where

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Prelude

-- Definitive mapping of presentation / legacy variants -> canonical IPA char.
-- Keys are characters that users commonly paste or type; values are the canonical
-- IPA codepoints used in the rest of the codebase (features maps, printing, etc).
--
-- Update this list if you accept more legacy inputs.
presentationVariants :: [(Char, Text)]
presentationVariants =
  [ ('I', "ɪ"), -- U+0049 LATIN CAPITAL LETTER I  -> U+026A LATIN SMALL LETTER CAPITAL I (IPA ɪ)
    ('Ɪ', "ɪ"), -- U+A78E LATIN CAPITAL LETTER SMALL CAPITAL I -> U+026A ɪ
    ('U', "ʊ"), -- U+0055 LATIN CAPITAL LETTER U -> U+028A LATIN SMALL LETTER UPSILON (IPA ʊ)
    ('Ꞹ', "ʊ"), -- U+A7B8 LATIN CAPITAL LETTER SMALL CAPITAL U -> U+028A ʊ
    ('ɷ', "ɔ"), -- U+0277 LATIN SMALL LETTER CLOSED OMEGA -> U+0254 LATIN SMALL LETTER OPEN O (ɔ)
    ('ɸ', "f"), -- U+0278 LATIN SMALL LETTER PHI -> U+0066 LATIN SMALL LETTER F (f)
    ('ɡ', "g"), -- U+0261 LATIN SMALL LETTER SCRIPT G -> U+0067 LATIN SMALL LETTER G (g)
    ('\865', ""), -- U+0361 COMBINING DOUBLE INVERTED BREVE (tie bar) is ignored
    -- strip tone letters
    ('\742', ""), -- U+02E6 MODIFIER LETTER EXTRA-HIGH TONE BAR
    ('\743', ""), -- U+02E7 MODIFIER LETTER HIGH TONE BAR
    ('\744', ""), -- U+02E8 MODIFIER LETTER MID TONE BAR
    ('\745', ""), -- U+02E9 MODIFIER LETTER LOW TONE BAR
    ('\746', ""), -- U+02EA MODIFIER LETTER EXTRA-LOW TONE BAR

    -- strip stress marks
    ('\712', ""), -- U+02C8 MODIFIER LETTER VERTICAL LINE (primary stress)
    ('\713', ""), -- U+02C9 MODIFIER LETTER MACRON (secondary stress)
    ('\714', ""), -- U+02CA MODIFIER LETTER ACUTE ACCENT (extra-high tone)
    ('\715', ""), -- U+02CB MODIFIER LETTER GRAVE ACCENT (extra-low tone)
    ('\716', ""), -- U+02CC MODIFIER LETTER LOW GRAVE ACCENT (extra-low tone)
    
    -- normalize length marks to U+02D0 MODIFIER LETTER TRIANGULAR COLON
    ('\720', "\720"), -- U+02D0 MODIFIER LETTER TRIANGULAR COLON
    ('\721', "\720"), -- U+02D1 MODIFIER LETTER HALF TRIANGULAR COLON
    -- Normalize ring diacritics to the combining ring below U+0310
    ('\778', "\805"), -- U+030A COMBINING RING ABOVE
    ('\805', "\805"), -- U+0310 COMBINING RING BELOW
    ('\858', "\805"), -- U+030B COMBINING DOUBLE RING BELOW
    -- seperate digraphs into two characters
    -- (these could be represented with tie bars, but we ignore those)
    -- consonant affricates
    ('ꞇ', "ts"), -- U+A787 LATIN SMALL LETTER T WITH STROKE -> U+0074 U+0073 (t + s)
    ('ʦ', "ts"), -- U+02A6 LATIN SMALL LETTER TS DIGRAPH -> U+0074 U+0073 (t + s)
    ('ʧ', "tʃ"), -- U+02A7 LATIN SMALL LETTER TC DIGRAPH -> U+0074 U+0283 (t + ʃ)
    ('ʨ', "tɕ"), -- U+02A8 LATIN SMALL LETTER TJ DIGRAPH -> U+0074 U+0255 (t + ɕ)
    ('ʤ', "dʒ"), -- U+02A4 LATIN SMALL LETTER DEZH DIGRAPH -> U+0064 U+0292 (d + ʒ)
    ('ʥ', "dz"), -- U+02A5 LATIN SMALL LETTER DZ DIGRAPH -> U+0064 U+007A (d + z)
    ('ʩ', "hŋ"), -- U+02A9 LATIN SMALL LETTER FENG DIGRAPH -> U+0066 U+014B (f + ŋ)
    ('ʪ', "ls"), -- U+02AA LATIN SMALL LETTER LS DIGRAPH -> U+006C U+0073 (l + s)
    ('ʫ', "lz") -- U+02AB LATIN SMALL LETTER LZ DIGRAPH -> U+006C U+007A (l + z)
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
