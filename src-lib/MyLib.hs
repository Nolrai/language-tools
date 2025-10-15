{-# LANGUAGE NoImplicitPrelude #-}
module MyLib (parseFile, intoSCL2) where
import Text.Trifecta
import Data.Set (Set)
import Data.Set qualified
import Text.Parser.Combinators
import GHC.Int
import Data.Maybe
import System.IO
import System.Exit (die)
import Data.Char
import Data.List ((++), notElem, any, null)
import Control.Applicative ((<|>), (*>), (<*), (<*>), (<$>), optional, pure)
import Control.Monad ((>>=), (>>), fail)
import Data.Function (($))
import Data.Eq
import Prelude (read, show)
import Data.ByteString (ByteString)
import GHC.Err

type String = [Char]

intoSCL2 :: [Entry] -> ByteString
intoSCL2 = error "stub for intoSCL2"

parseFile :: FilePath -> IO [Entry]
parseFile path = do
  result <- parseFromFileEx parseTable path
  case result of
    Failure err -> die (show err)
    Success value -> pure value

parseTable :: Parser [Entry]
parseTable = sepBy parseEntry newline

parseEntry :: Parser Entry
parseEntry = Entry <$> rankParser <*> spellingParser <*> linesParser

rankParser :: Parser Int -- either it's -1 or a nonnegative integer
rankParser =
  ((char '-' >> char '1' >> pure (-1)) <|> (read <$> some digit))
  <* char '\t'

-- Parses the standard spelling of a word, which is a quoted string of english letters.
spellingParser :: Parser String
spellingParser = char '"' *> some englishLetter <* char '"' <* char '\t'
  where
    englishLetter = satisfyRange 'a' 'z' <|> satisfyRange 'A' 'Z'

linesParser :: Parser [Line]
linesParser = char '"' *> sepBy1 lineParser newline <* char '"'

lineParser :: Parser Line
lineParser = Line <$> sepBy1 caseParser (string ", ") <*> notes

caseParser :: Parser Case
caseParser = Case <$> notes <*> ipaParser

wrapIn :: Char -> Parser a -> Char -> Parser a
wrapIn open p close = char open *> p <* char close

notes :: Parser [String]
notes = fromMaybe [] <$> optional noteList
  where
    noteList :: Parser [String]
    noteList = wrapIn '(' (sepBy1 noteParser (string ", ")) ')'
    noteParser :: Parser String
    noteParser = some (noneOf ",)")

ipaParser :: Parser String
ipaParser = do
  str <- wrapIn '/' (many (satisfy (/= '/'))) '/' <|> wrapIn '[' (many (satisfy (/= ']'))) ']'
  if null str
    then fail "Empty IPA transcription"
    else if any (`notElem` str) ipaChars
    then fail $ "Invalid characters in IPA transcription: " ++ str
    else pure str

ipaChars :: String
ipaChars = "I" -- UppercaseLetter
  ++ "abcdefghijklmnoprstuvwxyz" -- all the english lowercase letters except q
  ++ "äæçðøŋɐɑɒɔɘəɚɛɜɝɞɪɫɯɵɹɾʃʈʉʊʌʍʒθ" -- other LowercaseLetter
  ++ "ʰʱˈːˑ" -- ModifierLetter
  ++ "ʔ" -- OtherLetter -- note: this is a glottal stop, not a question mark
  -- these ones show up weirdly if you copy-paste from the source file, so I'm putting them in by codepoint
  ++ "\771\776\778\794\798\799\800\805\809\810\815\865" -- NonSpacingMark
  ++ "\742\743" -- ModifierSymbol

data Line = Line {cases :: [Case], lineNotes :: [String]}
data Case = Case {caseNotes :: [String], ipa :: String}

data Entry = Entry
  { rank :: Int
  , spelling :: String
  , lines :: [Line]
  }
