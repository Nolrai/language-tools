{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE RecordWildCards #-}
module MyLib (parseFile, intoSCL2, Entry(..), Case(..), Line(..)) where
import GHC.Int
import Data.Maybe
import System.IO ( IO, FilePath )
import Data.Char
import Data.Function (($))
import Data.ByteString (ByteString)
import GHC.Err
import Control.Monad.Except
import Text.Read (readMaybe)
import Control.Applicative
import Control.Monad (mapM, MonadFail (fail))
import Text.Trifecta
import Data.Text (Text, splitOn)
import Data.Text.IO (readFile)
import Data.Bool
import GHC.Show (Show(..))
import Data.Functor.Contravariant (Op(getOp))
import Data.Eq ((==))

type Text = [Char]

intoSCL2 :: [Entry] -> ByteString
intoSCL2 = error "stub for intoSCL2"

parseFile :: FilePath -> IO [Entry]
parseFile fileName = do
  entries <- splitOn "\n" <$> readFile fileName
  pure (parseEntry <$> entries)

parseEntry :: Text -> Except [Text] Entry
parseEntry entry = do
  let [rankStr, spelling, rest] = splitOn "\t" entry
  let Just rank = readMaybe rankStr
  let lines = splitOn ";" rest
  parsedLines <- mapM parseLine lines
  pure Entry {entryRank = rank, entrySpelling = spelling, entryLines = parsedLines}

data Token where
  Note :: {tokenString' :: Text} -> Token
  Ipa :: {ipaType :: IpaType, tokenString' :: Text} -> Token
  Semicolon :: Token

tokenString :: Token -> Except Text Text
tokenString (Note s) = pure s
tokenString (Ipa _ s) = pure s
tokenString Semicolon = fail "Semicolon has no string"

data IpaType = IpaSlashType | IpaSquareType

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
    go NoContext [] = pure []
    go NoContext ('(':cs) = go (NoteContext []) cs
    go NoContext (' ':cs) = go NoContext cs
    go NoContext (';':cs) = (Semicolon :) <$> go NoContext cs
    go NoContext (c:cs) =
        case matchOpenning c of
          Just ipaType -> go (IpaContext ipaType []) cs
          Nothing      -> fail $ "lexing failed: go NoContext " ++ show (c:cs)
    go (IpaContext ipaType sofar) (c : cs) = do
      case matchEnding c of
        Just endType ->
          if endType == ipaType
            then (Ipa ipaType (reverse sofar) :) <$> go NoContext cs
            else fail $ "ipa ended by wrong delimiter: " ++ show c
        Nothing ->
          if c `elem` ipaChars
            then go (IpaContext ipaType (c : sofar)) cs
            else fail $ "lexing failed: none Ipa character, go IpaContext " ++ show (c : cs)
    go (NoteContext sofar) (c : cs) =
      case c of
        ')' -> (Note (reverse sofar) :) <$> go NoContext cs
        ',' -> (Note (reverse sofar) :) <$> go (NoteContext []) cs
        _   -> go (NoteContext (c : sofar)) cs
    go _ [] = fail "lexing failed, unexpected end of line"

parseLine :: Text -> Except Text Line
parseLine str = do
  tokens <- toTokens str
  let (lineNotes, rest) = spanWhileNotes tokens
  cases <- parseCases rest
  pure Line {cases = reverse cases, lineNotes = lineNotes}

spanWhileNotes :: [Token] -> ([Text], [Token])
spanWhileNotes = go []
  where
    go acc (Note s : ts) = go (s : acc) ts
    go acc ts = (acc, ts)

parseCases :: [Token] -> Except Text [Case]
parseCases [] = pure []
parseCases (Ipa _ ipa : ts) = do
  let (caseNotes, rest) = spanWhileNotes ts
  let caseEntry = Case {caseNotes = reverse caseNotes, ipa = ipa}
  otherCases <- parseCases rest
  pure (caseEntry : otherCases)
parseCases (t : _) = fail $ "expected IPA token, got: " ++ show t


ipaChars :: Text
ipaChars = "IꞮ" -- UppercaseLetter
  ++ "abcdefghijklmnoprstuvwxyz" -- all the english lowercase letters except q
  ++ "äæçðøŋɐɑɒɔɘəɚɛɜɝɞɪɫɯɵɹɾʃʈʉʊʌʍʒθ" -- other LowercaseLetter
  ++ "ʰʱˈːˑ" -- ModifierLetter
  ++ "ʔ" -- OtherLetter -- note: this is a glottal stop, not a question mark
  -- these ones show up weirdly if you copy-paste from the source file, so I'm putting them in by codepoint
  ++ "\771\776\778\794\798\799\800\805\809\810\815\865" -- NonSpacingMark
  ++ "\742\743" -- ModifierSymbol

data Line = Line {cases :: [Case], lineNotes :: [Text]}
data Case = Case {caseNotes :: [Text], ipa :: Text}

data Entry = Entry
  { entryRank :: Int
  , entrySpelling :: Text
  , entryLines :: [Line]
  }
