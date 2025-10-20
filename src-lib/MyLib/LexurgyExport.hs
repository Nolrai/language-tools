{-# LANGUAGE OverloadedStrings #-}
-- Simple exporter that defines the Lexurgy feature definition preamble.
module MyLib.LexurgyExport
  ( lexurgyFeatureDeclarations
  , lexurgyDefinitions
  , writeLexurgyFeatures
  , Feature(..)
  , VowelFeature(..)
  , ConsonantFeature(..)
  , Height(..)
  , Backness(..)
  , Place(..)
  , Manner(..)
  , Voice(..)
  , FloatingFeature(..)
  , Length(..)
  , Stress(..)
  , PrePost(..)
  , LexurgyMeaning(..)
  ) where

import Prelude
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Char (chr, ord)

data Feature
  = VowelFeature VowelFeature
  | ConsonantFeature ConsonantFeature
  | Nazalized
  | Floating FloatingFeature
  | Syllabic Bool   -- ̩  (COMBINING VERTICAL LINE BELOW) : syllabic -- ̯  (COMBINING INVERTED BREVE BELOW) : non-syllabic
  | TieBar          -- ͡  (COMBINING DOUBLE INVERTED BREVE) : tie bar / affricate tie
  deriving (Eq, Show, Ord)

instance Bounded Feature where
  minBound = VowelFeature minBound
  maxBound = TieBar

instance Enum Feature where
  succ (VowelFeature vf) =
    if vf == maxBound then ConsonantFeature minBound else VowelFeature (succ vf)
  succ (ConsonantFeature cf) =
    if cf == maxBound then Nazalized else ConsonantFeature (succ cf)
  succ Nazalized = Floating minBound
  succ (Floating ff) =
    if ff == maxBound then Syllabic False else Floating (succ ff)
  succ (Syllabic b) =
    if b == maxBound then TieBar else Syllabic $ succ b
  succ TieBar = TieBar

  pred TieBar = Syllabic True
  pred (Syllabic b) =
    if b then Floating maxBound else Syllabic True
  pred (Floating ff) =
    if ff == minBound then Nazalized else Floating (pred ff)
  pred Nazalized = ConsonantFeature maxBound
  pred (ConsonantFeature cf) =
    if cf == minBound then VowelFeature maxBound else ConsonantFeature (pred cf)
  pred (VowelFeature vf) = VowelFeature (pred vf)

data VowelFeature
  = Height Height
  | Backness Backness
  | Rounding Bool
  | Centralized
  | Advanced        -- +̟  (COMBINING PLUS SIGN BELOW) : advanced / fronted
  | Retracted       -- ̠  (COMBINING MINUS SIGN BELOW) : retracted / backed
  | Lowered         -- ̞  (COMBINING DOWN TACK BELOW) : lowered (more open)
  | NonSyllabic     -- ̯  (COMBINING INVERTED BREVE BELOW) : non-syllabic
  | Rhotic          -- ɚ / ɝ variants: rhoticity
  deriving (Eq, Show, Ord)

instance Bounded VowelFeature where
  minBound = Height minBound
  maxBound = Rhotic

instance Enum VowelFeature where
  succ (Height h) =
    if h == maxBound then Backness minBound else Height (succ h)
  succ (Backness b) =
    if b == maxBound then Rounding minBound else Backness (succ b)
  succ (Rounding False) = Rounding True
  succ (Rounding True) = Centralized
  succ Centralized = Advanced
  succ Advanced = Retracted
  succ Retracted = Lowered
  succ Lowered = NonSyllabic
  succ NonSyllabic = Rhotic
  succ Rhotic = Rhotic  -- no successor

  pred Rhotic = NonSyllabic
  pred NonSyllabic = Lowered
  pred Lowered = Retracted
  pred Retracted = Advanced
  pred Advanced = Centralized
  pred Centralized = Rounding maxBound
  pred (Rounding True) = Rounding False
  pred (Rounding False) = Backness maxBound
  pred (Backness b) =
    if b == minBound then Height maxBound else Backness (pred b)
  pred (Height h) = Height (pred h)

data Height = Close | NearClose | CloseMid | Mid | OpenMid | NearOpen | Open
  deriving (Eq, Show, Enum, Ord, Bounded)

data Backness = Front | NearFront | Central | NearBack | Back
  deriving (Eq, Show, Enum, Ord, Bounded)

data ConsonantFeature
  = Place Place
  | Manner Manner
  | Voice Voice
  | Velarization
  | Unreleased
  | Apical          -- ̚ / ̯ variants: apical/articulatory narrow diacritic
  deriving (Eq, Show, Ord)

instance Bounded ConsonantFeature where
  minBound = Place minBound
  maxBound = Apical

instance Enum ConsonantFeature where
  succ (Place p) =
    if p == maxBound then Manner minBound else Place (succ p)
  succ (Manner m) =
    if m == maxBound then Voice minBound else Manner (succ m)
  succ (Voice v) =
    if v == maxBound then Velarization else Voice (succ v)
  succ Velarization = Unreleased
  succ Unreleased = Apical
  succ Apical = Apical

  pred Apical = Unreleased
  pred Unreleased = Velarization
  pred Velarization = Voice maxBound
  pred (Voice v) =
    if v == minBound then Manner maxBound else Voice (pred v)
  pred (Manner m) =
    if m == minBound then Place maxBound else Manner (pred m)
  pred (Place p) = Place (pred p)

data Place
  = Bilabial | Labiodental | InterDental | Dental | DentalAlveolar | Alveolar | Postalveolar | Retroflex
  | Palatal | Velar | Labiovelar | Glottal
  deriving (Eq, Show, Enum, Ord, Bounded)

data Manner
  = Stop | Nasal | Fricative | Approximant | LateralApproximant | Trill | Tap | FricativeApproximant
  deriving (Eq, Show, Enum, Ord, Bounded)

data Voice = Voiced | Voiceless
  deriving (Eq, Show, Enum, Ord, Bounded)

data FloatingFeature
  = Aspiration | BreathyVoice | Stress Stress | Length Length
  deriving (Eq, Show, Ord)

instance Bounded FloatingFeature where
  minBound = Aspiration
  maxBound = Length maxBound

instance Enum FloatingFeature where
  succ Aspiration = BreathyVoice
  succ (Stress s) = if s == maxBound then Length minBound else Stress (succ s)
  succ (Length l) = Length (succ l)

data Length = Half | Full | Long
  deriving (Eq, Show, Enum, Ord, Bounded)

data Stress = PrimaryStress | SecondaryStress | NoStress
  deriving (Eq, Show, Enum, Ord, Bounded)

data PrePost = Pre | Post
  deriving (Eq, Show, Enum, Ord, Bounded)

data LexurgyMeaning
  = LexurgySymbol (Set Feature)
  | LexurgyDiacritic (Set Feature) PrePost
  | LexurgyMeta
  deriving (Eq, Show)

featuresMap :: IntMap LexurgyMeaning
featuresMap = IntMap.fromList $
    -- vowels (height/back/round)
    [ mkVowel 'a'  [Height Open,   Backness Front,     Rounding False]
    , mkVowel 'ä'  [Height Open,   Backness Central,   Rounding False]
    , mkVowel 'æ'  [Height NearOpen,Backness Front,     Rounding False]
    , mkVowel 'e'  [Height CloseMid,Backness Front,     Rounding False]
    , mkVowel 'i'  [Height Close,  Backness Front,     Rounding False]
    , mkVowel 'Ɪ'  [Height NearClose, Backness NearFront, Rounding False]
    , mkVowel 'ɪ'  [Height NearClose, Backness NearFront, Rounding False]
    , mkVowel 'ɘ'  [Height CloseMid,  Backness Central,   Rounding False]
    , mkVowel 'ə'  [Height Mid,       Backness Central,   Rounding False]
    , mkVowel 'ɚ'  [Height Mid,       Backness Central,   Rhotic, Rounding False]
    , mkVowel 'ɜ'  [Height OpenMid,   Backness Central,   Rounding False]
    , mkVowel 'ɝ'  [Height OpenMid,   Backness Central,   Rhotic, Rounding False]
    , mkVowel 'ɵ'  [Height CloseMid,  Backness Central,   Rounding True]
    , mkVowel 'ʉ'  [Height Close,     Backness Central,   Rounding True]
    , mkVowel 'u'  [Height Close,     Backness Back,      Rounding True]
    , mkVowel 'ɯ'  [Height Close,     Backness Back,      Rounding False]
    , mkVowel 'o'  [Height CloseMid,  Backness Back,      Rounding True]
    , mkVowel 'ɒ'  [Height Open,      Backness Back,      Rounding True]
    , mkVowel 'ɑ'  [Height Open,      Backness Back,      Rounding False]
    , mkVowel 'ɔ'  [Height OpenMid,   Backness Back,      Rounding True]
    , mkVowel 'ʊ'  [Height NearClose,  Backness NearBack,  Rounding True]
    , mkVowel 'ʌ'  [Height OpenMid,   Backness Back,      Rounding False]
    , mkVowel 'ɞ'  [Height CloseMid,  Backness Central,   Rounding True]
    , mkVowel 'y'  [Height Close,     Backness Front,     Rounding True]
    ] ++
    -- consonants (place/manner/voice)
    [ mkConsonant 'p' [Place Bilabial, Manner Stop, Voice Voiceless]
    , mkConsonant 'b' [Place Bilabial, Manner Stop, Voice Voiced]
    , mkConsonant 't' [Place Alveolar, Manner Stop, Voice Voiceless]
    , mkConsonant 'd' [Place Alveolar, Manner Stop, Voice Voiced]
    , mkConsonant 'k' [Place Velar, Manner Stop, Voice Voiceless]
    , mkConsonant 'g' [Place Velar, Manner Stop, Voice Voiced]
    , mkConsonant 'ʈ' [Place Retroflex, Manner Stop, Voice Voiceless]
    , mkConsonant 'c' [Place Palatal, Manner Stop, Voice Voiceless]
    , mkConsonant 'm' [Place Bilabial, Manner Nasal, Voice Voiced]
    , mkConsonant 'n' [Place Alveolar, Manner Nasal, Voice Voiced]
    , mkConsonant 'ŋ' [Place Velar, Manner Nasal, Voice Voiced]
    , mkConsonant 'f' [Place Labiodental, Manner Fricative, Voice Voiceless]
    , mkConsonant 'v' [Place Labiodental, Manner Fricative, Voice Voiced]
    , mkConsonant 's' [Place Alveolar, Manner Fricative, Voice Voiceless]
    , mkConsonant 'z' [Place Alveolar, Manner Fricative, Voice Voiced]
    , mkConsonant 'ʃ' [Place Postalveolar, Manner Fricative, Voice Voiceless]
    , mkConsonant 'ʒ' [Place Postalveolar, Manner Fricative, Voice Voiced]
    , mkConsonant 'ç' [Place Palatal, Manner Fricative, Voice Voiceless]
    , mkConsonant 'x' [Place Velar, Manner Fricative, Voice Voiceless]
    , mkConsonant 'h' [Place Glottal, Manner Fricative, Voice Voiceless]
    , mkConsonant 'θ' [Place Dental, Manner Fricative, Voice Voiceless]
    , mkConsonant 'ð' [Place Dental, Manner Fricative, Voice Voiced]
    , mkConsonant 'l' [Place Alveolar, Manner LateralApproximant, Voice Voiced]
    , mkConsonant 'ɫ' [Place Alveolar, Manner LateralApproximant, Voice Voiced, Velarization]
    , mkConsonant 'ɹ' [Place Alveolar, Manner Approximant, Voice Voiced]
    , mkConsonant 'r' [Place Alveolar, Manner Trill, Voice Voiced]
    , mkConsonant 'ɾ' [Place Alveolar, Manner Tap, Voice Voiced]
    , mkConsonant 'w' [Place Labiovelar, Manner Approximant, Voice Voiced]
    , mkConsonant 'ʍ' [Place Labiovelar, Manner FricativeApproximant, Voice Voiceless]
    , mkConsonant 'j' [Place Palatal, Manner Approximant, Voice Voiced]
    , mkConsonant 'ʔ' [Place Glottal, Manner Stop, Voice Voiceless]
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
    , diaInt 778 [ConsonantFeature (Voice Voiceless)] Post    -- U+030A COMBINING RING ABOVE : voiceless
    , diaInt 794 [ConsonantFeature Apical] Post               -- U+031A COMBINING LEFT ANGLE ABOVE : apical (tongue-tip) marker
    , diaInt 798 [VowelFeature Lowered] Post                  -- U+031E COMBINING DOWN TACK BELOW : lowered (more open)
    , diaInt 799 [VowelFeature Advanced] Post                 -- U+031F COMBINING PLUS SIGN BELOW : advanced / fronted
    , diaInt 800 [VowelFeature Retracted] Post                -- U+0320 COMBINING MINUS SIGN BELOW : retracted / backed
    , diaInt 805 [ConsonantFeature (Voice Voiceless)] Post    -- U+0325 COMBINING RING BELOW : voiceless (below)
    , diaInt 809 [Syllabic True] Post             -- U+0329 COMBINING VERTICAL LINE BELOW : syllabic
    , diaInt 810 [ConsonantFeature (Place Dental)] Post       -- U+032A COMBINING BRIDGE BELOW : dental
    , diaInt 815 [VowelFeature NonSyllabic] Post              -- U+032F COMBINING INVERTED BREVE BELOW : non-syllabic
    , diaInt 865 [TieBar] Post               -- U+0361 COMBINING DOUBLE INVERTED BREVE : tie bar (affricate / affixation)
    ]
  where
    mkVowel :: Char -> [VowelFeature] -> (Int, LexurgyMeaning)
    mkConsonant :: Char -> [ConsonantFeature] -> (Int, LexurgyMeaning)
    mkVowel c vfs = sym c (Syllabic True : map VowelFeature vfs)
    mkConsonant c cfs = sym c (Syllabic False : map ConsonantFeature cfs)
    sym c fs = (ord c, LexurgySymbol (Set.fromList fs))
    dia c fs pos = (ord c, LexurgyDiacritic (Set.fromList fs) pos)
    meta c = (ord c, LexurgyMeta)
    symInt i fs = (i, LexurgySymbol (Set.fromList fs))
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
  , "Feature voice(unvoiced, voiced)"
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

c2t = T.singleton

showLexurgyMeaning :: Int -> LexurgyMeaning -> Text
showLexurgyMeaning code meaning =
  let c = chr code in
  case meaning of
    LexurgySymbol fs ->
      T.concat [ T.cons c "\t"
              , T.intercalate "\t" (map showFeature (Set.toList fs))
              ]
    LexurgyDiacritic fs pos ->
      T.concat [ T.cons c "\t"
              , T.intercalate "\t" (map showFeature (Set.toList fs))
              ]
    LexurgyDiacritic fs pos ->
      T.concat [ T.cons c "\t"
              , T.intercalate "\t" (map showFeature (Set.toList fs))
              , T.pack (show pos)
              ]
    LexurgyMeta -> T.pack "# " <> c2t c <> "\t"

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
    Centralized -> "centralized"
    Advanced -> "advanced"
    Retracted -> "retracted"
    Lowered -> "lowered"
    NonSyllabic -> "nonsyllabic"
  ConsonantFeature cf -> case cf of
    Place p -> case p of

    Manner m -> T.pack $ show m
    Voice v -> case v of
      Voiced -> "+voiced"
      Voiceless -> "-voiced"
    Velarization -> "velarized"
    Unreleased -> "unreleased"
    Apical -> "apical"
  Nazalized -> "nasalized"
  Floating ff -> case ff of
    Aspiration -> "aspiration"
    BreathyVoice -> "breathyvoice"
    Stress s -> case s of
      PrimaryStress -> "primarystress"
      SecondaryStress -> "secondarystress"
      NoStress -> "nostress"
    Length l -> case l of
      Half -> "half"
      Full -> "full"
      Long -> "long"

-- | Write the Lexurgy feature file to the given output path.
writeLexurgyFeatures :: FilePath -> IO ()
writeLexurgyFeatures outPath = do
  let contents = T.unlines []
  TIO.writeFile outPath contents