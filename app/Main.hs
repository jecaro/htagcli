module Main where

import AudioTrack qualified
import Check.File qualified as File
import Commands qualified
import Conduit ((.|))
import Conduit qualified
import Config qualified
import Data.Conduit.List qualified as Conduit
import Data.Either.Extra qualified as Either
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Options qualified
import Options.Applicative qualified as Options
import Path qualified
import Path.IO qualified as Path
import Pattern qualified
import Progress qualified
import System.Process.Typed qualified as Process
import Text.Megaparsec qualified as Megaparsec
import UnliftIO.Exception qualified as Exception
import UnliftIO.IO qualified as IO
import UnliftIO.Temporary qualified as Temporary

data Error
  = NoCheckInConfig
  | EditorExitError
  | ParseError (Megaparsec.ParseErrorBundle Text.Text Void)
  deriving (Show)

instance Exception.Exception Error

errorToText :: Error -> Text
errorToText NoCheckInConfig = "No checks provided in the config file"
errorToText EditorExitError = "The editor process exited with an error"
errorToText (ParseError parseError) =
  "Failed to parse the edited tags:\n"
    <> Text.pack (Megaparsec.errorBundlePretty parseError)

main :: IO ()
main = do
  command <- Options.execParser Options.optionsInfo

  Exception.handleAny exceptions $ do
    case command of
      Options.CreateConfig -> Config.createConfig
      Options.GetTags filesOrDirectory ->
        runConduitWithProgress
          filesOrDirectory
          $ Conduit.mapM_C Commands.getTags
      Options.SetTags setTagsOptions filesOrDirectory ->
        runConduitWithProgress
          filesOrDirectory
          $ Conduit.mapM_C
          $ Commands.setTags setTagsOptions
      Options.Edit filesOrDirectory -> do
        (editedContent, tempFilename) <- Temporary.withSystemTempFile "htagcli-edit-temp" $
          \tempFilename tempHandle -> do
            -- Write all input tags into a temporary file
            runConduitWithProgress
              filesOrDirectory
              $ Conduit.mapM getTagsAsText
                .| Conduit.sinkHandle tempHandle
            IO.hClose tempHandle

            -- Launch the editor
            editor <- liftIO $ fromMaybe "vim" <$> lookupEnv "EDITOR"
            let config = Process.proc editor [tempFilename]
            exitCode <- liftIO $ Process.runProcess config
            when
              (exitCode /= Process.ExitSuccess)
              (Exception.throwIO EditorExitError)

            -- Return the edited content
            content <- liftIO $ Text.readFile tempFilename
            pure (content, tempFilename)

        -- Parse the edited tags
        audioTracks <-
          Exception.fromEither $
            Either.mapLeft ParseError $
              Megaparsec.parse
                AudioTrack.audioTracksP
                tempFilename
                editedContent

        -- Set the new tags
        Progress.connectWithProgress
          (Conduit.yieldMany audioTracks)
          (Conduit.mapM_C AudioTrack.setTags)
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
            .| Conduit.groupOn (AudioTrack.atAlbum &&& AudioTrack.atDisc)
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
    getTagsAsText filename = do
      content <- encodeUtf8 . AudioTrack.asText <$> AudioTrack.getTags filename
      pure $ content <> "\n"

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
      (File.FilenameMatches pattern formatting@(Pattern.Formatting [] _ _)) =
        File.FilenameMatches
          pattern
          formatting {Pattern.foCharActions = charActions}
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
          errorToText mainException <> "\n"
      | Just configException <- fromException someException =
          Config.errorToText configException <> "\n"
      | Just commandsException <- fromException someException =
          Commands.errorToText commandsException <> "\n"
      | otherwise = "Unknown exception: " <> show someException <> "\n"
