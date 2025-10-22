{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module: HumanLanguage.Entry
-- Summary: Core data types for the language-tools package.
--
-- This module defines the small, plain data structures used across the
-- project (Entry, Line, Case). Parsing, IO and printing are intentionally
-- kept out of this module so that the types remain simple and easy to test.
--
-- Notes:
--  - NoImplicitPrelude is used to keep imports explicit and make dependencies
--    obvious; this file only depends on Data.Text for the textual fields.
--  - Keep these records stable: backwards-compatible changes are preferred.
module HumanLanguage.Entry
  ( Entry(..)
  , Line(..)
  , Case(..)
  ) where

import Data.Text (Text)
import Data.Int (Int)
import Data.Eq
import Text.Show (Show)

-- | A Line groups multiple Cases and any trailing notes for that line.
data Line = Line
  { cases :: [Case]
  , lineNotes :: [Text]
  } deriving (Eq, Show)

-- | A Case represents a single variant: optional notes and the IPA transcription.
data Case = Case
  { caseNotes :: [Text]  -- ^ notes that apply to this case (in-order)
  , ipa :: Text          -- ^ IPA transcription text for this case
  } deriving (Eq, Show)

-- | Top-level entry record: rank, spelling, and one or more lines.
data Entry = Entry
  { entryRank :: Int       -- ^ numerical rank/ID for the entry
  , entrySpelling :: Text  -- ^ orthographic spelling
  , entryLines :: [Line]   -- ^ associated lines for the entry
  } deriving (Eq, Show)