{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
-- |
-- Module: MyLib.EntryParser
-- Summary: Lexing and parsing for entries.
--
-- Responsibilities:
--  - Read a TSV-style file and produce [Entry].
--  - Tokenize individual line strings into Token values (Ipa, Note, separators).
--  - Parse tokens into Line and Case values.
--
-- Design notes / rationale:
--  - Parsing functions return Except Text to keep errors as textual messages
--    that bubble up to parseFile; parseFile aggregates errors and fails the IO
--    operation when any entry fails.
--  - Tokenization is conservative: unmatched delimiters and unexpected EOF
--    are reported as lexing errors rather than producing partial results.
--  - ipaChars is a whitelist chosen to avoid accepting arbitrary input inside
--    IPA spans; update it if you need to support more unicode categories.

module MyLib.EntryParser
  ( parseFile
  , parseFileFull
  , parseEntry
  , toTokens
  , parseLine
  , Token(..)
  , tokenString
  , IpaType(..)
  ) where

import MyLib.Entry (Entry(..), Line(..), Case(..))

import Prelude

import Control.Monad.Except (Except, runExcept, throwError, withExcept)
import Control.Exception (throwIO)
import Text.Read (readMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Either (partitionEithers)
import qualified Data.List as List
import Data.IntSet (IntSet)
import qualified Data.IntSet as Set

-- | Convert Showable value to Text.
tshow :: Show a => a -> Text
tshow = T.pack . show

-- | Default preview length for user-facing error messages.
defaultPreviewLen :: Int
defaultPreviewLen = 100

-- | Truncate to a specific length, appending "..." on truncation.
shortenTo :: Int -> Text -> Text
shortenTo n txt
  | T.length txt > n = T.take n txt <> "..."
  | otherwise        = txt

-- | Truncate to defaultPreviewLen and append "..." when truncated.
shorten :: Text -> Text
shorten = shortenTo defaultPreviewLen

-- | Which kind of IPA delimiter was used: /.../ or [...].
data IpaType = IpaSlashType | IpaSquareType
  deriving (Eq, Show)

-- | Lexical token types produced by 'toTokens'.
data Token where
  -- | Inline note text (from parentheses).
  Note :: { noteString :: Text } -> Token
  -- | IPA span with delimiter type and inner text.
  Ipa  :: { ipaType :: IpaType, ipaString :: Text } -> Token
  -- | Field separator between "lines".
  Semicolon :: Token
  -- | Field separator between "cases".
  Comma :: Token
  deriving (Eq, Show)

-- | Extract text from a token; Semicolon/Comma are an error.
tokenString :: Token -> Except Text Text
tokenString (Note s) = pure s
tokenString (Ipa _ s) = pure s
tokenString Semicolon = throwError "Semicolon has no string"
tokenString Comma = throwError "Comma has no string"

-- | Map an opening delimiter character to its IpaType.
-- NOTE: name corrected from the earlier typo matchOpenning -> matchOpening.
matchOpening :: Char -> Maybe IpaType
matchOpening '/' = Just IpaSlashType
matchOpening '[' = Just IpaSquareType
matchOpening _   = Nothing

-- | Map a closing delimiter character to its IpaType.
matchEnding :: Char -> Maybe IpaType
matchEnding '/' = Just IpaSlashType
matchEnding ']' = Just IpaSquareType
matchEnding _   = Nothing

-- helper token-scan context; kept at module scope but not exported
data TokenContext = IpaContext IpaType Text | NoteContext Text | NoContext

-- | Tokenize a line of Text into Note, Ipa, Comma and Semicolon tokens.
-- Note: tokens produced by 'toTokens' are in left-to-right (input) order.
-- Later, getEndNotes intentionally reverses the token list so that trailing
-- Note tokens can be collected easily; parseCases expects tokens in
-- reversed order (see comment near 'getEndNotes' and 'parseCases').
toTokens :: Text -> Except Text [Token]
toTokens = go NoContext
  where
    go :: TokenContext -> Text -> Except Text [Token]
    go NoContext txt
      | T.null txt = pure []
      | otherwise =
          case T.uncons txt of
            Just ('(', cs) -> go (NoteContext T.empty) cs
            Just (' ', cs) -> go NoContext cs
            Just (';', cs) -> (Semicolon :) <$> go NoContext cs
            Just (',', cs) -> (Comma :) <$> go NoContext cs
            Just (c, cs) ->
              case matchOpening c of
                Just ipaType -> go (IpaContext ipaType T.empty) cs
                Nothing -> throwError $ "lexing failed: unexpected char in NoContext: " <> T.singleton c <> " in " <> txt
            Nothing -> pure []

    go (IpaContext ipaType sofar) txt =
      case T.uncons txt of
        Nothing -> throwError "lexing failed, unexpected end of line inside IPA"
        Just (c, cs) ->
          case matchEnding c of
            Just endType ->
              if endType == ipaType
                then (Ipa ipaType (T.reverse sofar) :) <$> go NoContext cs
                else throwError $ "ipa ended by wrong delimiter: " <> T.singleton c <> " in " <> txt
            Nothing ->
              if isIPAChar c
                then go (IpaContext ipaType (T.cons c sofar)) cs
                else throwError $ "lexing failed: invalid IPA character " <> T.singleton c <> " in " <> txt

    go (NoteContext sofar) txt =
      case T.uncons txt of
        Nothing -> throwError "lexing failed, unexpected end of line inside note"
        Just (c, cs) ->
          case c of
            ')' -> (Note (T.reverse sofar) :) <$> go NoContext cs
            ',' -> (Note (T.reverse sofar) :) <$> go (NoteContext T.empty) cs
            _   -> go (NoteContext (T.cons c sofar)) cs

-- | Parse a tokenized line into a Line: cases and trailing notes.
parseLine :: Text -> Except Text Line
parseLine str = do
  tokens <- toTokens str
  let (lineNotes, rest) = getEndNotes tokens
  cases' <- parseCases rest
  pure Line { cases = List.reverse cases', lineNotes = lineNotes }

-- Collects trailing Note tokens and returns the remaining tokens
getEndNotes :: [Token] -> ([Text], [Token])
getEndNotes = spanWhileNotes . List.reverse

spanWhileNotes :: [Token] -> ([Text], [Token])
spanWhileNotes = go []
  where
    go acc (Note s : ts) = go (s : acc) ts
    go acc ts = (acc, ts)

-- expects the tokens in reverse order
parseCases :: [Token] -> Except Text [Case]
parseCases [] = pure []
parseCases (Ipa _ ipaText : ts) = do
  let (caseNotes, rest) = spanWhileNotes ts
  let caseEntry = Case { caseNotes = List.reverse caseNotes, ipa = ipaText }
  rest' <-
    case rest of
      [] -> pure []
      (Comma : rest') -> pure rest'
      _ -> throwError $ "expected Comma or end of tokens, got: " <> shorten (tshow rest)
  otherCases <- parseCases rest'
  pure (caseEntry : otherCases)
parseCases (t : _) = throwError $ "expected IPA token, got: " <> shorten (tshow t)

isIPAChar :: Char -> Bool
isIPAChar c = Set.member (fromEnum c) ipaSet

-- | Allowed characters inside IPA spans (letters, modifiers, combining marks, etc.).
ipaSet :: IntSet
ipaSet = Set.fromList (List.map fromEnum (T.unpack ipaChars))
  where
  ipaChars :: Text
  ipaChars = T.concat
    [ "IꞮ" -- UppercaseLetter
    , "abcdefghijklmnoprstuvwxyz" -- english lowercase letters (q omitted)
    , "äæçðøŋɐɑɒɔɘəɚɛɜɝɞɪɫɯɵɹɾʃʈʉʊʌʍʒθ" -- other letters
    , "ʰʱˈːˑ" -- modifier letters
    , "ʔ" -- glottal stop
    , T.pack "\771\776\778\794\798\799\800\805\809\810\815\865" -- NonSpacingMark codepoints
    , T.pack "\742\743" -- ModifierSymbol
    , ".()" -- Puctuation
    ]

-- | Read a tab-separated input file and parse each entry; if there are parse
-- errors, return them (full) via Left so callers can choose how to present them.
parseFileFull :: FilePath -> IO (Either [(Int, Text)] [Entry])
parseFileFull fileName = do
  contents <- TIO.readFile fileName
  let entries = List.filter (not . T.null) $ T.splitOn "\n" contents
  let (errors, successes) =
        partitionEithers
          . List.map runExcept
          . List.zipWith (\i -> withExcept (i,)) [(0 :: Int)..]
          $ parseEntry <$> entries
  if List.null errors
    then pure (Right successes)
    else pure (Left errors)

-- | Convenience wrapper used by the existing codepath:
-- throws an IO error with a user-friendly (truncated) message when parsing fails,
-- otherwise returns the parsed entries.
-- Truncation is applied here (user-facing), full errors are available via 'parseFileFull'.
parseFile :: FilePath -> IO [Entry]
parseFile fileName = do
  r <- parseFileFull fileName
  case r of
    Right successes -> pure successes
    Left errors -> do
      let toString (i, err) = T.unpack $ "Error in entry " <> tshow i <> ": " <> shorten err <> "\n"
      throwIO (userError (List.unlines (List.map toString errors)))

-- | Parse one entry line into an Entry (rank, spelling, lines).
-- Expects three tab-separated fields: rank, spelling, rest.
parseEntry :: Text -> Except Text Entry
parseEntry entry =
  case T.splitOn "\t" entry of
    [rankStr, spelling, rest] ->
      case readMaybe (T.unpack rankStr) of
        Just rank -> do
          let rawLines = T.splitOn ";" rest
          parsedLines <- mapM parseLine rawLines
          pure Entry { entryRank = rank, entrySpelling = spelling, entryLines = parsedLines }
        Nothing -> throwError $ "invalid rank: " <> rankStr
    _ -> throwError $ "malformed entry (expected 3 tab fields): " <> entry

