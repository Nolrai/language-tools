{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module MyLib (parseFile, intoSCL2, Entry(..), Case(..), Line(..), Token(..), tokenString) where
import GHC.Int
import Data.Maybe
import System.IO ( IO, FilePath )
import Data.Char
import Data.Function (($), (.))
import Data.ByteString (ByteString)
import GHC.Err
import Control.Monad.Except
import Text.Read (readMaybe)
import Control.Applicative
import Control.Monad (mapM, unless)
import Data.Text (Text, splitOn, unpack)
import Data.Text.IO (readFile)
import qualified Data.Text as T
import Data.Bool (otherwise)
import Data.Eq (Eq((==)))
import Data.Monoid ((<>))
import Data.Either (partitionEithers)
import qualified Data.List as List
import GHC.Base (failIO)
import GHC.Show (Show)

intoSCL2 :: [Entry] -> ByteString
intoSCL2 = error "stub for intoSCL2"

parseFile :: FilePath -> IO [Entry]
parseFile fileName = do
  entries <- splitOn "\n" <$> readFile fileName
  let (errors, successes) =
        partitionEithers
          . List.map runExcept
          . List.zipWith (\ i -> withExcept (i,)) [(0 :: Int)..]
          $ parseEntry <$> entries
  let toString (i, err) = unpack $ "Error in entry " <> T.show i <> ": " <> err <> "\n"
  unless (List.null errors) $ failIO (List.unlines . List.map toString $ errors)
  pure successes

parseEntry :: Text -> Except Text Entry
parseEntry entry = do
  let [rankStr, spelling, rest] = splitOn "\t" entry
  let Just rank = readMaybe (unpack rankStr)
  let lines = splitOn ";" rest
  parsedLines <- mapM parseLine lines
  pure Entry {entryRank = rank, entrySpelling = spelling, entryLines = parsedLines}

data Token where
  Note :: {tokenString' :: Text} -> Token
  Ipa :: {ipaType :: IpaType, tokenString' :: Text} -> Token
  Semicolon :: Token
  deriving (Show)

tokenString :: Token -> Except Text Text
tokenString (Note s) = pure s
tokenString (Ipa _ s) = pure s
tokenString Semicolon = throwError "Semicolon has no string"

data IpaType = IpaSlashType | IpaSquareType
  deriving (Eq, Show)

matchOpenning :: Char -> Maybe IpaType
matchOpenning '/' = Just IpaSlashType
matchOpenning '[' = Just IpaSquareType
matchOpenning _   = Nothing

matchEnding :: Char -> Maybe IpaType
matchEnding '/' = Just IpaSlashType
matchEnding ']' = Just IpaSquareType
matchEnding _   = Nothing

data TokenContext where
  IpaContext :: IpaType -> Text -> TokenContext
  NoteContext :: Text -> TokenContext
  NoContext :: TokenContext

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
            Just (c, cs) ->
              case matchOpenning c of
                Just ipaType -> go (IpaContext ipaType T.empty) cs
                Nothing -> throwError $ "lexing failed: go NoContext " <> T.show txt
            Nothing -> pure []
    go (IpaContext ipaType sofar) txt =
      case T.uncons txt of
        Nothing -> throwError "lexing failed, unexpected end of line"
        Just (c, cs) ->
          case matchEnding c of
            Just endType ->
              if endType == ipaType
                then (Ipa ipaType (T.reverse sofar) :) <$> go NoContext cs
                else throwError $ "ipa ended by wrong delimiter: " <> T.show c <> " in " <> T.show (T.unpack txt)
            Nothing ->
              if T.elem c ipaChars
                then go (IpaContext ipaType (T.cons c sofar)) cs
                else throwError $ "lexing failed: none Ipa character, go IpaContext " <> T.show txt
    go (NoteContext sofar) txt =
      case T.uncons txt of
        Nothing -> throwError "lexing failed, unexpected end of line"
        Just (c, cs) ->
          case c of
            ')' -> (Note (T.reverse sofar) :) <$> go NoContext cs
            ',' -> (Note (T.reverse sofar) :) <$> go (NoteContext T.empty) cs
            _   -> go (NoteContext (T.cons c sofar)) cs


parseLine :: Text -> Except Text Line
parseLine str = do
  tokens <- toTokens str
  let (lineNotes, rest) = getEndNotes tokens
  cases <- parseCases rest
  pure Line {cases = List.reverse cases, lineNotes = lineNotes}

-- Collects leading Note tokens and returns their strings and the rest of the tokens
-- The note strings are output not in reverse order -- because they were collected in reverse order
-- the other tokens are reversed
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
parseCases (Ipa _ ipa : ts) = do
  let (caseNotes, rest) = spanWhileNotes ts
  let caseEntry = Case {caseNotes = List.reverse caseNotes, ipa = ipa}
  otherCases <- parseCases rest
  pure (caseEntry : otherCases)
parseCases (t : _) = throwError $ "expected IPA token, got: " <> T.show t

ipaChars :: Text
ipaChars = "IꞮ" -- UppercaseLetter
  <> "abcdefghijklmnoprstuvwxyz" -- all the english lowercase letters except q
  <> "äæçðøŋɐɑɒɔɘəɚɛɜɝɞɪɫɯɵɹɾʃʈʉʊʌʍʒθ" -- other LowercaseLetter
  <> "ʰʱˈːˑ" -- ModifierLetter
  <> "ʔ" -- OtherLetter -- note: this is a glottal stop, not a question mark
  -- these ones show up weirdly if you copy-paste from the source file, so I'm putting them in by codepoint
  <> "\771\776\778\794\798\799\800\805\809\810\815\865" -- NonSpacingMark
  <> "\742\743" -- ModifierSymbol

data Line = Line {cases :: [Case], lineNotes :: [Text]}
data Case = Case {caseNotes :: [Text], ipa :: Text}

data Entry = Entry
  { entryRank :: Int
  , entrySpelling :: Text
  , entryLines :: [Line]
  }
