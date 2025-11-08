module Main where

import AudioTrack qualified
import Check.File qualified as File
import Commands qualified
import Conduit ((.|))
import Conduit qualified
import Config qualified
import Data.Conduit.List qualified as Conduit
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Options qualified
import Options.Applicative qualified as Options
import Path qualified
import Path.IO qualified as Path
import Pattern qualified
import Progress qualified
import UnliftIO.Exception qualified as Exception

data Error = NoCheckInConfig
  deriving (Show)

instance Exception.Exception Error

render :: Error -> Text
render NoCheckInConfig = "No checks provided in the config file"

main :: IO ()
main = do
  command <- Options.execParser Options.optionsInfo

  Exception.handleAny exceptions $ do
    case command of
      Options.Display filesOrDirectory ->
        runConduitWithProgress
          filesOrDirectory
          $ Conduit.mapM_C Commands.display
      Options.Edit editOptions filesOrDirectory ->
        runConduitWithProgress
          filesOrDirectory
          $ Conduit.mapM_C
          $ Commands.edit editOptions
      Options.Check options filesOrDirectory -> do
        config <- Config.readConfig

        -- Get the checks from the CLI and fallback to the config file
        let (fileChecks, albumChecks) = withDefaults config options

        when (null fileChecks && null albumChecks) $
          Exception.throwIO NoCheckInConfig

        runConduitWithProgress
          filesOrDirectory
          $ Conduit.mapM AudioTrack.getTags
            .| Conduit.iterM (Commands.checkFile fileChecks)
            .| Conduit.groupOn AudioTrack.atAlbum
            .| Conduit.mapM_C (Commands.checkAlbum albumChecks)
      Options.FixFilePaths Options.FixFilePathsOptions {..} filesOrDirectory -> do
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

        runConduitWithProgress
          filesOrDirectory
          $ Conduit.mapM_C
          $ Commands.fixFilePaths fixFilePathOptions
  where
    -- When no check is given on the CLI, fallback to the config ones
    withDefaults config (Options.CheckOptions [] []) = Config.checks config
    -- If the formatting option is empty in the 'FileMatches' check, fallback
    -- on the value in the config
    withDefaults config (Options.CheckOptions files albums) =
      (setCharActions foCharActions <$> files, albums)
      where
        Config.Config
          { coFilename = Config.Filename {fiFormatting = Pattern.Formatting {..}}
          } = config

    setCharActions
      charActions
      (File.FilenameMatches pattern (Pattern.Formatting [] padding)) =
        File.FilenameMatches
          pattern
          (Pattern.Formatting charActions padding)
    setCharActions _ check = check

runConduitWithProgress ::
  Options.FilesOrDirectory ->
  Conduit.ConduitT
    (Path.Path Path.Abs Path.File)
    Void
    (Conduit.ResourceT IO)
    a ->
  IO a
runConduitWithProgress = Progress.connectWithProgress . fileOrDirectoryC

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
