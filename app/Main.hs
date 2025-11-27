module Main where

import Commands qualified
import Conduit ((.|))
import Conduit qualified
import Config qualified
import Data.Conduit.Combinators qualified as Conduit
import Data.Either.Extra qualified as Either
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Model.Album qualified as Album
import Model.Artist qualified as Artist
import Model.AudioTrack qualified as AudioTrack
import Options qualified
import Options.Applicative qualified as Options
import Path qualified
import Path.IO qualified as Path
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
        let (trackChecks, albumChecks, mbArtistCheck) = Options.checks config options

        when (null trackChecks && null albumChecks && null mbArtistCheck) $
          Exception.throwIO NoCheckInConfig

        runConduitWithProgress
          filesOrDirectory
          $ Conduit.mapM AudioTrack.getTags
            .| Conduit.iterM (Commands.checkTrack trackChecks)
            .| albumC
            .| Conduit.iterM (Commands.checkAlbum albumChecks)
            .| artistC
            .| Conduit.mapM_C (Commands.checkArtist mbArtistCheck)
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

albumC ::
  (Monad m) =>
  Conduit.ConduitT AudioTrack.AudioTrack Album.Album m ()
albumC = clusterC Album.mkAlbum Album.addTrack

artistC ::
  (Monad m) =>
  Conduit.ConduitT Album.Album Artist.Artist m ()
artistC = clusterC Artist.mkArtist Artist.addAlbum

-- | Cluster incoming items into groups using the provided 'mk' and 'add'
-- functions
clusterC ::
  (Monad m) =>
  (NonEmpty i -> Maybe o) ->
  (i -> o -> Maybe o) ->
  Conduit.ConduitT i o m ()
clusterC mk add = loop Nothing
  where
    loop Nothing =
      Conduit.await >>= \case
        Nothing -> pure ()
        -- Create a new cluster
        Just item -> loop $ mk $ item :| []
    loop (Just cluster) =
      Conduit.await >>= \case
        Nothing -> Conduit.yield cluster
        Just item -> case add item cluster of
          Just newCluster -> loop $ Just newCluster
          Nothing -> do
            Conduit.yield cluster
            loop $ mk $ item :| []

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
