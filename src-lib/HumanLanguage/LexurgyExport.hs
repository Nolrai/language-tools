{-# LANGUAGE OverloadedStrings #-}
-- Simple exporter that defines the Lexurgy feature definition preamble.
module HumanLanguage.LexurgyExport
  ( lexurgyPrelude
  , featuresMap
  , lexurgyPath
  ) where

import Prelude

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Char (chr, ord)
import qualified Data.Foldable
import HumanLanguage.LexurgyTypes
import HumanLanguage.LexurgyInstances ()

featuresMap :: IntMap LexurgyMeaning
featuresMap = IntMap.fromList $
    -- vowels (height/back/round)
    [ mkVowel 'a'   [Height Open,      Backness Front,    Rounding False]
    , mkVowel 'ä'   [Height Open,      Backness Central,  Rounding False]
    , mkVowel 'æ'   [Height NearOpen,  Backness Front,    Rounding False]
    , mkVowel 'e'   [Height CloseMid,  Backness Front,    Rounding False]
    , mkVowel 'ɛ'   [Height OpenMid,   Backness Front,    Rounding False]
    , mkVowel 'ø'   [Height CloseMid,  Backness Front,    Rounding True ]
    , mkVowel 'ɨ'   [Height Close,     Backness Central,  Rounding False]
    , mkVowel 'ɐ'   [Height NearOpen,  Backness Central,  Rounding False]
    , mkVowel 'œ'   [Height OpenMid,   Backness Front,    Rounding True ]
    , mkVowel 'ɶ'   [Height Open,      Backness Front,    Rounding True ]
    , mkVowel 'ɤ'   [Height CloseMid,  Backness Back,     Rounding False]
    , mkVowel 'i'   [Height Close,     Backness Front,    Rounding False]
    , mkVowel 'Ɪ'   [Height NearClose, Backness NearFront, Rounding False]
    , mkVowel 'ɪ'   [Height NearClose, Backness NearFront, Rounding False]
    , mkVowel 'ɘ'   [Height CloseMid,  Backness Central,  Rounding False]
    , mkVowel 'ə'   [Height Mid,       Backness Central,  Rounding False]
    , mkVowel 'ɚ'   [Height Mid,       Backness Central,  Rhotic, Rounding False]
    , mkVowel 'ɜ'   [Height OpenMid,   Backness Central,  Rounding False]
    , mkVowel 'ɝ'   [Height OpenMid,   Backness Central,  Rhotic, Rounding False]
    , mkVowel 'ɵ'   [Height CloseMid,  Backness Central,  Rounding True ]
    , mkVowel 'ʉ'   [Height Close,     Backness Central,  Rounding True ]
    , mkVowel 'u'   [Height Close,     Backness Back,     Rounding True ]
    , mkVowel 'ɯ'   [Height Close,     Backness Back,     Rounding False]
    , mkVowel 'o'   [Height CloseMid,  Backness Back,     Rounding True ]
    , mkVowel 'ɒ'   [Height Open,      Backness Back,     Rounding True ]
    , mkVowel 'ɑ'   [Height Open,      Backness Back,     Rounding False]
    , mkVowel 'ɔ'   [Height OpenMid,   Backness Back,     Rounding True ]
    , mkVowel 'ʊ'   [Height NearClose, Backness NearBack,  Rounding True ]
    , mkVowel 'ʌ'   [Height OpenMid,   Backness Back,     Rounding False]
    , mkVowel 'ɞ'   [Height CloseMid,  Backness Central,  Rounding True ]
    , mkVowel 'y'   [Height Close,     Backness Front,    Rounding True ]
    ] ++
    -- consonants (place/manner/voice)
    [ mkConsonant 'p' [Place Bilabial,      Manner Stop,                 Voice Voiceless]
    , mkConsonant 'b' [Place Bilabial,      Manner Stop,                 Voice Voiced   ]
    , mkConsonant 't' [Place Alveolar,      Manner Stop,                 Voice Voiceless]
    , mkConsonant 'd' [Place Alveolar,      Manner Stop,                 Voice Voiced   ]
    , mkConsonant 'k' [Place Velar,         Manner Stop,                 Voice Voiceless]
    , mkConsonant 'g' [Place Velar,         Manner Stop,                 Voice Voiced   ]
    , mkConsonant 'ʈ' [Place Retroflex,     Manner Stop,                 Voice Voiceless]
    , mkConsonant 'c' [Place Palatal,       Manner Stop,                 Voice Voiceless]
    , mkConsonant 'm' [Place Bilabial,      Manner Nasal,                Voice Voiced   ]
    , mkConsonant 'n' [Place Alveolar,      Manner Nasal,                Voice Voiced   ]
    , mkConsonant 'ŋ' [Place Velar,         Manner Nasal,                Voice Voiced   ]
    , mkConsonant 'f' [Place Labiodental,   Manner Fricative,            Voice Voiceless]
    , mkConsonant 'v' [Place Labiodental,   Manner Fricative,            Voice Voiced   ]
    , mkConsonant 's' [Place Alveolar,      Manner Fricative,            Voice Voiceless]
    , mkConsonant 'z' [Place Alveolar,      Manner Fricative,            Voice Voiced   ]
    , mkConsonant 'ʃ' [Place Postalveolar,  Manner Fricative,            Voice Voiceless]
    , mkConsonant 'ʒ' [Place Postalveolar,  Manner Fricative,            Voice Voiced   ]
    , mkConsonant 'ç' [Place Palatal,       Manner Fricative,            Voice Voiceless]
    , mkConsonant 'x' [Place Velar,         Manner Fricative,            Voice Voiceless]
    , mkConsonant 'h' [Place Glottal,       Manner Fricative,            Voice Voiceless]
    , mkConsonant 'θ' [Place Dental,        Manner Fricative,            Voice Voiceless]
    , mkConsonant 'ð' [Place Dental,        Manner Fricative,            Voice Voiced   ]
    , mkConsonant 'l' [Place Alveolar,      Manner LateralApproximant,   Voice Voiced   ]
    , mkConsonant 'ɫ' [Place Alveolar,      Manner LateralApproximant,   Voice Voiced   , Velarization]
    , mkConsonant 'ɹ' [Place Alveolar,      Manner Approximant,          Voice Voiced   ]
    , mkConsonant 'r' [Place Alveolar,      Manner Trill,                Voice Voiced   ]
    , mkConsonant 'ɾ' [Place Alveolar,      Manner Tap,                  Voice Voiced   ]
    , mkConsonant 'w' [Place Labiovelar,    Manner Approximant,          Voice Voiced   ]
    , mkConsonant 'ʍ' [Place Labiovelar,    Manner FricativeApproximant, Voice Voiceless]
    , mkConsonant 'j' [Place Palatal,       Manner Approximant,          Voice Voiced   ]
    , mkConsonant 'ʔ' [Place Glottal,       Manner Stop,                 Voice Voiceless]
    ] ++
    -- suprasegmentals / diacritics
    [ dia 'ʰ' [Floating Aspiration] Post
    , dia 'ʱ' [Floating BreathyVoice] Post
    , dia 'ˈ' [Floating (Stress PrimaryStress)] Pre
    , dia 'ː' [Floating (Length Long)] Post
    , dia 'ˑ' [Floating (Length Half)] Post
    ] ++
    -- punctuation / notes (symbol rows with empty feature sets)
    [ meta '.'
    , meta '('
    , meta ')'
    ] ++
    -- combining marks
    [ diaInt 771 [Nazalized] Post                -- U+0303 COMBINING TILDE : nasalization
    , diaInt 776 [VowelFeature Centralized] Post              -- U+0308 COMBINING DIAERESIS : centralized / centralized vowel
    , diaInt 805 [ConsonantFeature (Voice Voiceless)] Post    -- U+0310 COMBINING RING BELOW : voiceless
    , diaInt 794 [ConsonantFeature Apical] Post               -- U+031A COMBINING LEFT ANGLE ABOVE : apical (tongue-tip) marker
    , diaInt 798 [VowelFeature Lowered] Post                  -- U+031E COMBINING DOWN TACK BELOW : lowered (more open)
    , diaInt 799 [VowelFeature Advanced] Post                 -- U+031F COMBINING PLUS SIGN BELOW : advanced / fronted
    , diaInt 800 [VowelFeature Retracted] Post                -- U+0320 COMBINING MINUS SIGN BELOW : retracted / backed
    , diaInt 809 [Syllabic True] Post             -- U+0329 COMBINING VERTICAL LINE BELOW : syllabic
    , diaInt 810 [ConsonantFeature (Place Dental)] Post       -- U+032A COMBINING BRIDGE BELOW : dental
    , diaInt 815 [Syllabic False] Post              -- U+032F COMBINING INVERTED BREVE BELOW : non-syllabic
    ]

  where
    mkVowel :: Char -> [VowelFeature] -> (Int, LexurgyMeaning)
    mkConsonant :: Char -> [ConsonantFeature] -> (Int, LexurgyMeaning)
    mkVowel c vfs = sym c (Syllabic True : map VowelFeature vfs)
    mkConsonant c cfs = sym c (Syllabic False : map ConsonantFeature cfs)
    sym c fs = (ord c, LexurgySymbol (Set.fromList fs))
    dia c fs pos = (ord c, LexurgyDiacritic (Set.fromList fs) pos)
    meta c = (ord c, LexurgyMeta)
    diaInt i fs pos = (i, LexurgyDiacritic (Set.fromList fs) pos)

lexurgyFeatureDeclarations :: Text
lexurgyFeatureDeclarations = T.unlines
  [ "# Feature declarations for lexurgy"
  , "Feature syllabic"
  , "# Vowel features"
  , "Feature height(close, nearclose, closemid, mid, openmid, nearopen, open)"
  , "Feature backness(front, nearfront, central, nearback, back)"
  , "Feature rounded"
  , "Feature nasalized"
  , "Feature +centralized"
  , "Feature +advanced"
  , "Feature +retracted"
  , "Feature +lowered"
  , "# Consonant features"
  , "Feature place(bilabial, labiodental, interdental, dental, dentalalveolar, alveolar, postalveolar, retroflex, palatal, velar, labiovelar, glottal)"
  , "Feature manner(stop, nasal, fricative, approximant, lateralapproximant, trill, tap, fricativeapproximant)"
  , "Feature voiced"
  , "Feature +velarized"
  , "Feature +unreleased"
  , "Feature +apical"
  , "# Floating features"
  , "Feature +aspiration"
  , "Feature +breathyvoice"
  , "Feature stress(primarystress, secondarystress, nostress)"
  , "Feature length(half, full, long)"
  ]

lexurgyDefinitions :: Text
lexurgyDefinitions = T.unlines $ map (uncurry showLexurgyMeaning) $ IntMap.toList featuresMap

(<:) :: Char -> Text -> Text
c <: t = T.cons c t

-- Helpers for rendering
posText :: PrePost -> Text
posText Pre = " (before)"
posText Post = "" -- intentionally left blank for after

-- space-separated feature list
renderFeatures :: Set Feature -> Text
renderFeatures fs = T.intercalate " " $ map showFeature (Set.toList fs)

showLexurgyMeaning :: Int -> LexurgyMeaning -> Text
showLexurgyMeaning code meaning =
  let c = chr code in
  case meaning of
    LexurgySymbol fs ->
      "symbol " <> c <: " [" <> renderFeatures fs <> "]"
    LexurgyDiacritic fs pos ->
      "diacritic " <> c <: posText pos <> floatingText fs <> " [" <> renderFeatures fs <> "] "
    LexurgyMeta -> "# " <> c <: " (meta)"

floatingText :: Set Feature -> Text
floatingText fs =
  if Data.Foldable.any isFloating fs
    then " (floating)"
    else ""
  where
    isFloating :: Feature -> Bool
    isFloating (Floating _) = True
    isFloating _            = False

showFeature :: Feature -> Text
showFeature f = case f of
  VowelFeature vf -> case vf of
    Height h -> case h of
      Close -> "close"
      NearClose -> "nearclose"
      CloseMid -> "closemid"
      Mid -> "mid"
      OpenMid -> "openmid"
      NearOpen -> "nearopen"
      Open -> "open"
    Backness b -> case b of
      Front -> "front"
      NearFront -> "nearfront"
      Central -> "central"
      NearBack -> "nearback"
      Back -> "back"
    Rounding False -> "-rounded"
    Rounding True -> "+rounded"
    Centralized -> "+centralized"
    Advanced -> "+advanced"
    Retracted -> "+retracted"
    Lowered -> "+lowered"
    Rhotic -> "+rhotic"
  ConsonantFeature cf -> case cf of
    Place p -> case p of
      Bilabial -> "bilabial"
      Labiodental -> "labiodental"
      InterDental -> "interdental"
      Dental -> "dental"
      DentalAlveolar -> "dentalalveolar"
      Alveolar -> "alveolar"
      Postalveolar -> "postalveolar"
      Retroflex -> "retroflex"
      Palatal -> "palatal"
      Velar -> "velar"
      Labiovelar -> "labiovelar"
      Glottal -> "glottal"
    Manner m -> case m of
      Stop -> "stop"
      Nasal -> "nasal"
      Fricative -> "fricative"
      Approximant -> "approximant"
      LateralApproximant -> "lateralapproximant"
      Trill -> "trill"
      Tap -> "tap"
      FricativeApproximant -> "fricativeapproximant"
    Voice v -> case v of
      Voiced -> "+voiced"
      Voiceless -> "-voiced"
    Velarization -> "+velarized"
    Unreleased -> "+unreleased"
    Apical -> "+apical"
  Nazalized -> "+nasalized"
  Floating ff -> case ff of
    Aspiration -> "+aspiration"
    BreathyVoice -> "+breathyvoice"
    Stress s -> case s of
      PrimaryStress -> "primarystress"
      SecondaryStress -> "secondarystress"
      NoStress -> "nostress"
    Length l -> case l of
      Half -> "half"
      Full -> "full"
      Long -> "long"
  Syllabic b -> if b then "+syllabic" else "-syllabic"
  TieBar -> "tiebar"

lexurgyPrelude :: Text
lexurgyPrelude = T.unlines
  [ "# Lexurgy prelude definitions"
  , lexurgyFeatureDeclarations
  , lexurgyDefinitions
  ]

lexurgyPath :: FilePath
lexurgyPath = "/home/chris/myprojects/language-tools/lexurgy"