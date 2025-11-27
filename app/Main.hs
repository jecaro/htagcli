module Main where

import Commands qualified
import ConduitUtils qualified
import Config qualified
import Data.Conduit.Combinators qualified as Conduit
import Data.Either.Extra qualified as Either
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Model.AudioTrack qualified as AudioTrack
import Options qualified
import Options.Applicative qualified as Options
import Path.IO qualified as Path
import Progress qualified
import Stats qualified
import System.Exit qualified as System
import System.Process.Typed qualified as Process
import Text.Megaparsec qualified as Megaparsec
import UnliftIO.Exception qualified as Exception
import UnliftIO.IO qualified as IO
import UnliftIO.Temporary qualified as Temporary
import "conduit" Conduit ((.|))
import "conduit" Conduit qualified

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
        ConduitUtils.runConduitWithProgress
          filesOrDirectory
          $ Conduit.mapM_C Commands.getTags
      Options.SetTags setTagsOptions filesOrDirectory ->
        ConduitUtils.runConduitWithProgress
          filesOrDirectory
          $ Conduit.mapM_C
          $ Commands.setTags setTagsOptions
      Options.Edit filesOrDirectory -> do
        (editedContent, tempFilename) <- Temporary.withSystemTempFile "htagcli-edit-temp" $
          \tempFilename tempHandle -> do
            -- Write all input tags into a temporary file
            ConduitUtils.runConduitWithProgress
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

        stats <- newIORef Stats.empty
        let modifyStats = modifyIORef' stats
            addTrackErrors = modifyStats . Stats.addTrackErrors
            addAlbumErrors = modifyStats . Stats.addAlbumErrors
            incArtistErrors = modifyStats Stats.incArtistErrors

        ConduitUtils.runConduitWithProgress
          filesOrDirectory
          ( Conduit.mapM AudioTrack.getTags
              .| Conduit.iterM
                (addTrackErrors <=< Commands.checkTrack trackChecks)
              .| ConduitUtils.albumC
              .| Conduit.iterM
                (addAlbumErrors <=< Commands.checkAlbum albumChecks)
              .| ConduitUtils.artistC
              .| Conduit.mapM_C
                (flip when incArtistErrors <=< Commands.checkArtist mbArtistCheck)
          )

        Stats.CheckErrors {..} <- readIORef stats
        unless (null trackChecks) $
          putTextLn $
            "Track errors: " <> show ceTrackErrors
        unless (null albumChecks) $
          putTextLn $
            "Album errors: " <> show ceAlbumErrors
        when (isJust mbArtistCheck) $
          putTextLn $
            "Artist errors: " <> show ceArtistErrors

        let total = ceTrackErrors + ceAlbumErrors + ceArtistErrors
        when (total > 0) $ System.exitWith $ System.ExitFailure total
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

        ConduitUtils.runConduitWithProgress
          filesOrDirectory
          $ Conduit.mapM_C
          $ Commands.fixFilePaths fixFilePathOptions
  where
    getTagsAsText filename = do
      content <- encodeUtf8 . AudioTrack.asText <$> AudioTrack.getTags filename
      pure $ content <> "\n"

exceptions :: SomeException -> IO ()
exceptions someException
  -- Rethrow exit failures to preserve the exit code
  | Just (err :: System.ExitCode) <- fromException someException =
      Exception.throwIO err
  | otherwise = do
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
