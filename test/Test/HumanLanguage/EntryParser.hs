{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.HumanLanguage.EntryParser (tests) where

import Prelude (IO, ($), String, not)
import Control.Monad.Except (runExcept)
import System.IO.Temp (withSystemTempFile)
import System.IO (hClose)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Either (isLeft, Either (..))
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import Data.List qualified as List

import HumanLanguage.Entry (Entry(..), entryRank, entrySpelling, entryLines, cases, ipa)
import HumanLanguage.EntryParser (parseFile, Token(..), tokenString, IpaType(..))
import Control.Exception (SomeException, try)
import Control.Applicative (Applicative(..))

tests :: TestTree
tests = testGroup "EntryParser"
  [ testGroup "tokenString"
    [ testCase "Note returns underlying text" $
        runExcept (tokenString (Note ("a note" :: T.Text)))
        @?= Right ("a note" :: T.Text)

    , testCase "Ipa returns underlying text" $
        runExcept (tokenString (Ipa IpaSlashType ("ipa" :: T.Text)))
        @?= Right ("ipa" :: T.Text)

    , testCase "Semicolon errors" $
        runExcept (tokenString Semicolon)
        @?= Left ("Semicolon has no string" :: T.Text)

    , QC.testProperty "Note tokenString is identity for printable strings" $
        \(s :: String) ->
        runExcept (tokenString (Note (T.pack s))) === Right (T.pack s)
    ]

  , testGroup "parseFile"
    [ testCase "parses minimal valid file with one entry and one IPA case" $
        withSystemTempFile "valid.tsv" $ \path h -> do
          TIO.hPutStr h ("1\tword\t/ipa/\n" :: T.Text)
          hClose h
          entries <- parseFile path
          -- basic sanity checks
          assertBool "entries non-empty" (not (List.null entries))
          e : _ <- pure entries
          entryRank e @?= 1
          entrySpelling e @?= ("word" :: T.Text)
          assertBool "entryLines non-empty" (not (List.null (entryLines e)))
          l : _ <- pure (entryLines e)
          assertBool "cases non-empty" (not (List.null (cases l)))
          c : _ <- pure (cases l)
          ipa c @?= ("ipa" :: T.Text)

    , testCase "parseFile fails when any entry fails to parse" $
        withSystemTempFile "invalid.tsv" $ \path h -> do
          TIO.hPutStr h ("this\tis\n" :: T.Text)
          hClose h
          -- parseFile is expected to throw; catch exceptions and assert failure occurred
          r <- try (parseFile path) :: IO (Either SomeException [Entry])
          assertBool "parseFile should throw on malformed entry" (isLeft r)
    ]

  , testGroup "tokenization"
    [ testCase "tokenString handles Note and Ipa together" $
        let n = Note ("(comment)" :: T.Text)
            i = Ipa IpaSlashType ("tɪst" :: T.Text)
        in do
          runExcept (tokenString n) @?= Right ("(comment)" :: T.Text)
          runExcept (tokenString i) @?= Right ("tɪst" :: T.Text)

    , testCase "tokenString returns Left for non-string tokens (Semicolon)" $
        runExcept (tokenString Semicolon) @?= Left ("Semicolon has no string" :: T.Text)
    ]
  ]
