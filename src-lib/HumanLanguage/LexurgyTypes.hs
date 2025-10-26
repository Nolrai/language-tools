module HumanLanguage.LexurgyTypes
  ( Feature(..)
  , VowelFeature(..)
  , Height(..)
  , Backness(..)
  , ConsonantFeature(..)
  , Place(..)
  , Manner(..)
  , Voice(..)
  , FloatingFeature(..)
  , Length(..)
  , Stress(..)
  , PrePost(..)
  , LexurgyMeaning(..)
  ) where

import GHC.Generics (Generic)
import Data.Set (Set)

-- Top-level feature sum (vowel / consonant / floating / syllabic / tiebar)
data Feature
  = VowelFeature VowelFeature
  | ConsonantFeature ConsonantFeature
  | Nasalized
  | Floating FloatingFeature
  | Syllabic Bool
  | TieBar
  deriving (Eq, Show, Ord, Generic)

-- Vowel-related features
data VowelFeature
  = Height Height
  | Backness Backness
  | Rounding Bool
  | Centralized
  | Advanced
  | Retracted
  | Lowered
  | Rhotic
  deriving (Eq, Show, Ord, Generic)

data Height = Close | NearClose | CloseMid | Mid | OpenMid | NearOpen | Open
  deriving (Eq, Show, Enum, Ord, Bounded)

data Backness = Front | NearFront | Central | NearBack | Back
  deriving (Eq, Show, Enum, Ord, Bounded)

-- Consonant-related features
data ConsonantFeature
  = Place Place
  | Manner Manner
  | Voice Voice
  | Velarization
  | Unreleased
  | Apical
  deriving (Eq, Show, Ord, Generic)

data Place
  = Bilabial | Labiodental | InterDental | Dental | DentalAlveolar | Alveolar
  | Postalveolar | Retroflex | Palatal | Velar | Labiovelar | Glottal
  deriving (Eq, Show, Enum, Ord, Bounded)

data Manner
  = Stop | Nasal | Fricative | Approximant | LateralApproximant | Trill | Tap | FricativeApproximant
  deriving (Eq, Show, Enum, Ord, Bounded)

data Voice = Voiced | Voiceless
  deriving (Eq, Show, Enum, Ord, Bounded)

-- Floating / suprasegmental features
data FloatingFeature
  = Aspiration | BreathyVoice | Stress Stress | Length Length
  deriving (Eq, Show, Ord, Generic)

data Length = Half | Full | Long
  deriving (Eq, Show, Enum, Ord, Bounded)

data Stress = PrimaryStress | SecondaryStress | NoStress
  deriving (Eq, Show, Enum, Ord, Bounded)

-- diacritic position
data PrePost = Pre | Post
  deriving (Eq, Show, Enum, Ord, Bounded)

-- runtime meaning of a codepoint in the lexurgy file
data LexurgyMeaning
  = LexurgySymbol (Set Feature)
  | LexurgyDiacritic (Set Feature) PrePost
  | LexurgyMeta
  deriving (Eq, Show, Ord, Generic)