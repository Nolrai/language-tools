{-# LANGUAGE OverloadedStrings #-}
module HumanLanguage.IPA where

import Data.Text (Text)
import qualified Data.Text as T
import Data.IntSet (IntSet)
import qualified Data.IntSet as Set
import qualified Data.List as List

-- | Check if a character is allowed inside IPA spans.
isIPAChar :: Char -> Bool
isIPAChar c = Set.member (fromEnum c) ipaSet

-- | Allowed characters inside IPA spans (letters, modifiers, combining marks, etc.).
ipaSet :: IntSet
ipaSet = Set.fromList (List.map fromEnum (T.unpack ipaChars))

-- | Text of allowed IPA characters.
ipaChars :: Text
ipaChars = T.concat
  [ "IꞮ" -- UppercaseLetter
  , "abcdefghijklmnoprstuvwxyz" -- english lowercase letters (q omitted)
  , "äæçðøŋɐɑɒɔɘəɚɛɜɝɞɪɫɯɵɹɾʃʈʉʊʌʍʒθ" -- other letters
  , "ʰʱˈːˑ" -- modifier letters
  , "ʔ" -- glottal stop
  , T.pack "\771\776\778\794\798\799\800\805\809\810\815" -- NonSpacingMark codepoints
  , T.pack "\742\743" -- ModifierSymbol
  , "." -- SylableBreak
  , "()" -- parentheses for optional segments
  ]

