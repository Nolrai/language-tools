module RunLexurgy where

import System.Process (callProcess, showCommandForUser)
import System.IO (FilePath, IO)

runLexurgy :: HasCallStack => [FilePath] -> FilePath -> IO ()
runLexurgy words ruleFile = do
  let arguments = ["sc", "-o", getBaseName ruleFile, ruleFile] ++ words
  putStrLn $ "Running lexurgy: " <> showCommandForUser lexurgyPath arguments
  callProcess lexurgyPath arguments