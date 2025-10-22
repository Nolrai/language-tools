{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module HumanLanguage.PrintToLexurgy (intoLexurgy) where

-- Module: HumanLanguage.PrintToLexurgy
-- Summary: Conversion of parsed Entries to Lexurgy ByteString.
--
-- This module produces a UTF-8 encoded ByteString suitable for Lexurgy.
-- For now the conversion is a simple textual formatting.

import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.List as List
import Data.Semigroup (Semigroup(..))

import HumanLanguage.Entry (Entry(..), Line(..), Case(..))
import Data.Function (($))

-- | Convert parsed entries into Lexurgy ByteString output.
-- the output is UTF-8 encoded text
intoLexurgy :: [Entry] -> ByteString
intoLexurgy entries =
  let txt = T.intercalate "\n\n" (List.map fromEntry entries)
  in encodeUtf8 txt

-- a Lexurgy item consists of an ipa transcription and optional notes
-- each entry converts into one or more Lexurgy items
-- items are separated by newlines
-- entries are separated by double newlines

fromEntry :: Entry -> Text
fromEntry Entry{..} =
  T.intercalate "\n" (List.map (fromLine entrySpelling) entryLines)

fromLine :: Text -> Line -> Text
fromLine spelling Line{..} =
  T.intercalate "\n" (List.map (fromCase spelling lineNotes) cases)

fromCase :: Text -> [Text] -> Case -> Text
fromCase spelling lineNotes Case{..} =
  let notesAll = caseNotes <> lineNotes
      -- notes can contain "/" which Lexurgy interprets as a separator, so replace with "["
      notesText = T.replace "/" "[" $ if List.null notesAll then "" else " (" <> T.intercalate ", " notesAll <> ")"
  in ipa <> "\t(" <> "Gloss: " <> spelling  <> ", " <> notesText <> ")"