#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Process
import System.IO
import Data.Char (toUpper)


main :: IO ()
main = do
  wordListText <- readFile "wordlist.txt"
  let wordList = lines wordListText
  wordListIpa <- readFile "wordlist_ipa.txt"
  let ipaList = lines wordListIpa
  let pairedList = zip wordList ipaList
  let formattedLines = map formatLine pairedList
  writeFile "formatted_wordlist.txt" (unlines formattedLines)

formatLine :: (String, String) -> String
formatLine (word, ipa) = (toUpper <$> word) ++ " " ++ ipa