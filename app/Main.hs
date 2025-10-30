module Main where

import AudioTrack qualified
import Check qualified
import Conduit ((.|))
import Conduit qualified
import Config qualified
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Options qualified
import Options.Applicative qualified as Options
import Path ((</>))
import Path qualified
import Path.IO qualified as Path
import Pattern qualified
import Sound.HTagLib qualified as HTagLib
import Sound.HTagLib.Extra qualified as HTagLib
import UnliftIO.Exception qualified as Exception

data Error
  = NoCheckInConfig
  | UnableToFormatFile (Path.Path Path.Abs Path.File)
  deriving (Show)

instance Exception.Exception Error

render :: Error -> Text.Text
render NoCheckInConfig = "No checks provided in the config file"
render (UnableToFormatFile file) = "Unable to format file: " <> show file

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
            .| Conduit.mapM_C
              (putTextLn . AudioTrack.asText <=< AudioTrack.getTags)
      Options.Edit Options.EditOptions {..} -> do
        Conduit.runConduitRes $
          fileOrDirectoryC opFilesOrDirectory
            .| Conduit.mapM_C
              ( \filename -> do
                  let setter =
                        fold $
                          catMaybes
                            [ HTagLib.titleSetter <$> eoTitle,
                              HTagLib.artistSetter <$> eoArtist,
                              HTagLib.albumSetter <$> eoAlbum,
                              HTagLib.albumArtistSetter <$> eoAlbumArtist,
                              HTagLib.genreSetter <$> eoGenre,
                              toSetter HTagLib.yearSetter eoYear,
                              toSetter HTagLib.trackNumberSetter eoTrack
                            ]
                  HTagLib.setTags (Path.toFilePath filename) Nothing setter
              )
      Options.Check (Options.CheckOptions {..}) -> do
        -- Get the checks from the CLI and fallback to the config file
        checks <- maybe getChecksFromConfig pure coChecks
        Conduit.runConduitRes $
          fileOrDirectoryC opFilesOrDirectory
            .| Conduit.mapM_C
              ( \filename -> do
                  track <- AudioTrack.getTags filename
                  traverse_ (checkPrintError filename track) checks
              )
      Options.FixFilePaths Options.FixFilePathsOptions {..} -> do
        Config.Config {coFilename = Config.Filename {..}, ..} <- Config.readConfig
        let formatting = fromMaybe fiFormatting foFormatting
            pattern = fromMaybe fiPattern foPattern
        -- Get the base directory from the cli and fallback to the config file
        baseDir <- maybe (pure coFixPaths) Path.makeAbsolute foBaseDirectory

        Conduit.runConduitRes $
          fileOrDirectoryC opFilesOrDirectory
            .| Conduit.mapM_C
              ( \fromFile -> do
                  track <- AudioTrack.getTags fromFile
                  toFile <-
                    Exception.fromEither $
                      maybeToRight (UnableToFormatFile fromFile) $
                        Pattern.toPath formatting track pattern
                  let toFileAbs = baseDir </> toFile
                  when (toFileAbs /= fromFile) $ do
                    putTextLn $
                      "Moving: " <> show fromFile <> " to " <> show toFileAbs
                    when foDryRun $ do
                      Path.createDirIfMissing True (Path.parent toFileAbs)
                      Path.renameFile fromFile toFileAbs
              )
  where
    getChecksFromConfig = do
      config <- Config.readConfig
      maybe (Exception.throwIO NoCheckInConfig) pure $
        nonEmpty $
          Config.checks config
    toSetter _ Nothing = Nothing
    toSetter setter (Just Options.Remove) = Just $ setter Nothing
    toSetter setter (Just (Options.Set v)) = Just . setter $ Just v
    checkPrintError file track check =
      whenLeft_ (Check.check check track) $ \err ->
        putTextLn $ fromString (Path.toFilePath file) <> ": " <> Check.render err

exceptions :: SomeException -> IO ()
exceptions someException = do
  Text.hPutStr stderr message
  exitFailure
  where
    message
      | Just configException <- fromException someException =
          Config.render configException <> "\n"
      | Just mainException <- fromException someException =
          render mainException <> "\n"
      | otherwise = "Unknown exception: " <> show someException <> "\n"
