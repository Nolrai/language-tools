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
-- |
-- | Preconditions:
-- |  * 'lexurgyPath' must point to an executable available to the process.
-- |  * 'ruleFile' should be a path to an existing, readable file.
-- |
-- | Behaviour:
-- |  * Prints the command being run, then invokes the external process via
-- |    'callProcess'. Any 'IOException' produced by the process invocation is
-- |    propagated to the caller.
-- |
-- | Note: this function is not partial in Haskell terms, but will fail at
-- | runtime if the external command is missing or the file is unreadable.
runLexurgy :: [FilePath] -> FilePath -> IO ()
runLexurgy extraArgs ruleFile = do
  let arguments = ["sc", "-o", takeBaseName ruleFile, ruleFile] <> extraArgs
  putStrLn $ "Running lexurgy: " <> showCommandForUser lexurgyPath arguments
  callProcess lexurgyPath arguments

-- | Wrapper exception carrying additional textual context and the original exception.
-- |
-- | Fields:
-- |  * 'context' — a short, human-readable description of the operation that failed
-- |    (prefer a single line and avoid including secrets).
-- |  * 'originalException' — the caught 'SomeException' that triggered this wrapper.
-- |
-- | Use this type to annotate exceptions at IO boundaries so higher-level code can
-- | report contextual information while preserving the original cause.
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
-- |
-- | Preconditions:
-- |  * 'ctx' should describe the operation (e.g. \"reading config file <path>\")
-- |    and must NOT contain sensitive data.
-- |
-- | Behaviour:
-- |  * Catches all exceptions of type 'SomeException' and rethrows an
-- |    'AnnotatedException' that includes both the provided context and the
-- |    original exception. The original exception is preserved in
-- |    'originalException' for programmatic inspection.
-- |
-- | Recommended usage:
-- |  * Use at top-level IO boundaries (file reads, process invocation) to add
-- |    human-friendly diagnostics before propagating errors to error-reporting
-- |    layers or test assertions.
annotateIO :: Text -> IO a -> IO a
annotateIO ctx action =
  action `catch` \(e :: SomeException) ->
    throwIO (AnnotatedException {context = ctx, originalException = e})
