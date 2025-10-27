{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Small IO helpers.
-- | This module hosts utilities for invoking external tools; RunLexurgy was
-- | moved here to centralize IO helpers used by multiple executables.
module Utils.IO
  ( runLexurgy,
    annotateIO,
  )
where

import Control.Exception (Exception, SomeException, catch, displayException, throwIO)
import Data.Function (($))
import Data.Semigroup ((<>))
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Show (Show (..))
import HumanLanguage.LexurgyExport (lexurgyPath)
import System.FilePath (FilePath, takeBaseName)
import System.IO (IO, putStrLn)
import System.Process (callProcess, showCommandForUser)

-- | Run the `lexurgy` executable with the given extra args and rule file.
-- | Prints the command being run, then calls the executable.
runLexurgy :: [FilePath] -> FilePath -> IO ()
runLexurgy extraArgs ruleFile = do
  let arguments = ["sc", "-o", takeBaseName ruleFile, ruleFile] <> extraArgs
  putStrLn $ "Running lexurgy: " <> showCommandForUser lexurgyPath arguments
  callProcess lexurgyPath arguments

-- | Lightweight exception wrapper that annotates an arbitrary exception with context.
data AnnotatedException
  = AnnotatedException
  { context :: Text,
    originalException :: SomeException
  }
  deriving (Show)

instance Exception AnnotatedException where
  displayException AnnotatedException {..} =
    "Error during: " <> T.unpack context <> "\n" <> displayException originalException

-- | Run an IO action and, on exception, rethrow it wrapped with a short context message.
annotateIO :: Text -> IO a -> IO a
annotateIO ctx action =
  action `catch` \(e :: SomeException) ->
    throwIO (AnnotatedException {context = ctx, originalException = e})
