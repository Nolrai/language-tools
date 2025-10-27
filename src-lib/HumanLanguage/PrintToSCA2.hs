{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HumanLanguage.PrintToSCA2 (intoSCA2) where

-- Module: HumanLanguage.PrintToSCA2
-- Summary: Conversion of parsed Entries to SCA² ByteString.
--
-- This module produces a UTF-8 encoded ByteString suitable for SCA².
-- For now the conversion is a simple textual formatting.

import Data.ByteString (ByteString)
import Data.Function (($))
import Data.List qualified as List
import Data.Semigroup (Semigroup (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import HumanLanguage.Entry (Case (..), Entry (..), Line (..))

-- | Convert parsed entries into SCA² ByteString output.
-- the output is UTF-8 encoded text
intoSCA2 :: [Entry] -> ByteString
intoSCA2 entries =
  let txt = T.intercalate "\n\n" (List.map fromEntry entries)
   in encodeUtf8 txt

-- a SCA² item consists of an ipa transcription and optional notes
-- each entry converts into one or more SCA² items
-- items are separated by newlines
-- entries are separated by double newlines

fromEntry :: Entry -> Text
fromEntry Entry {..} =
  T.intercalate "\n" (List.map (fromLine entrySpelling) entryLines)

fromLine :: Text -> Line -> Text
fromLine spelling Line {..} =
  T.intercalate "\n" (List.map (fromCase spelling lineNotes) cases)

fromCase :: Text -> [Text] -> Case -> Text
fromCase spelling lineNotes Case {..} =
  let notesAll = caseNotes <> lineNotes
      -- notes can contain "/" which SCA² interprets as a separator, so replace with "["
      notesText = T.replace "/" "[" $ if List.null notesAll then "" else " (" <> T.intercalate ", " notesAll <> ")"
   in ipa <> " ‣ " <> "Gloss: " <> spelling <> notesText
