module Main where

import Commands qualified
import Conduit ((.|))
import Conduit qualified
import Config qualified
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Options qualified
import Options.Applicative qualified as Options
import Path qualified
import Path.IO qualified as Path
import UnliftIO.Exception qualified as Exception

data Error = NoCheckInConfig
  deriving (Show)

instance Exception.Exception Error

render :: Error -> Text.Text
render NoCheckInConfig = "No checks provided in the config file"

fileOrDirectoryC ::
  (Conduit.MonadResource m, Conduit.MonadThrow m) =>
  Options.FilesOrDirectory ->
  Conduit.ConduitT i (Path.Path Path.Abs Path.File) m ()
fileOrDirectoryC (Options.FDFiles (Options.Files {..})) = do
  absFiles <- traverse Path.makeAbsolute fiFiles
  Conduit.yieldMany absFiles
fileOrDirectoryC (Options.FDDirectory (Options.Directory {..})) = do
  absDir <- Path.makeAbsolute diPath
  Conduit.sourceDirectoryDeep False (Path.toFilePath absDir)
    .| Conduit.filterC
      (\filename -> any (`Text.isSuffixOf` fromString filename) diExtensions)
    .| Conduit.mapMC Path.parseAbsFile

main :: IO ()
main = do
  Options.Options {..} <- Options.execParser Options.optionsInfo

  Exception.handleAny exceptions $ do
    case opCommand of
      Options.Display -> do
        Conduit.runConduitRes $
          fileOrDirectoryC opFilesOrDirectory
            .| Conduit.mapM_C Commands.display
      Options.Edit editOptions -> do
        Conduit.runConduitRes $
          fileOrDirectoryC opFilesOrDirectory
            .| Conduit.mapM_C (Commands.edit editOptions)
      Options.Check (Options.CheckOptions {..}) -> do
        -- Get the checks from the CLI and fallback to the config file
        checks <- maybe getChecksFromConfig pure coChecks
        Conduit.runConduitRes $
          fileOrDirectoryC opFilesOrDirectory
            .| Conduit.mapM_C (Commands.check checks)
      Options.FixFilePaths Options.FixFilePathsOptions {..} -> do
        Config.Config {coFilename = Config.Filename {..}, ..} <- Config.readConfig
        let formatting = fromMaybe fiFormatting foFormatting
            pattern = fromMaybe fiPattern foPattern
        -- Get the base directory from the cli and fallback to the config file
        baseDir <- maybe (pure coFixPaths) Path.makeAbsolute foBaseDirectory
        let fixFilePathOptions =
              Commands.FixFilePathsOptions
                { Commands.fiDryRun = foDryRun,
                  Commands.fiBaseDirectory = baseDir,
                  Commands.fiFormatting = formatting,
                  Commands.fiPattern = pattern
                }

        Conduit.runConduitRes $
          fileOrDirectoryC opFilesOrDirectory
            .| Conduit.mapM_C (Commands.fixFilePath fixFilePathOptions)
  where
    getChecksFromConfig = do
      config <- Config.readConfig
      maybe (Exception.throwIO NoCheckInConfig) pure $
        nonEmpty $
          Config.checks config

exceptions :: SomeException -> IO ()
exceptions someException = do
  Text.hPutStr stderr message
  exitFailure
  where
    message
      | Just mainException <- fromException someException =
          render mainException <> "\n"
      | Just configException <- fromException someException =
          Config.render configException <> "\n"
      | Just commandsException <- fromException someException =
          Commands.render commandsException <> "\n"
      | otherwise = "Unknown exception: " <> show someException <> "\n"
