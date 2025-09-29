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
import Path qualified
import Sound.HTagLib qualified as HTagLib
import UnliftIO.Exception qualified as Exception

fileOrDirectoryC ::
  (Conduit.MonadResource m, Conduit.MonadThrow m) =>
  Options.FilesOrDirectory ->
  Conduit.ConduitT i (Path.SomeBase Path.File) m ()
fileOrDirectoryC (Options.FDFiles (Options.Files {..})) =
  Conduit.yieldMany fiFiles
fileOrDirectoryC (Options.FDDirectory (Options.Directory {..})) =
  Conduit.sourceDirectoryDeep False (Path.prjSomeBase Path.toFilePath diPath)
    .| Conduit.filterC
      (\filename -> any (`Text.isSuffixOf` fromString filename) diExtensions)
    .| Conduit.mapMC Path.parseSomeFile

main :: IO ()
main = do
  options <- Options.execParser Options.optionsInfo

  Exception.handleAny exceptions $ do
    case options of
      Options.Display Options.DisplayOptions {..} -> do
        Conduit.runConduitRes $
          fileOrDirectoryC doFilesOrDirectory
            .| Conduit.mapM_C
              (putTextLn . AudioTrack.render <=< AudioTrack.getTags)
      Options.Edit Options.EditOptions {..} -> do
        Conduit.runConduitRes $
          fileOrDirectoryC eoFilesOrDirectory
            .| Conduit.mapM_C
              ( \file -> do
                  let filename = Path.prjSomeBase Path.toFilePath file
                      setter =
                        fold $
                          catMaybes
                            [ HTagLib.titleSetter <$> eoTitle,
                              HTagLib.artistSetter <$> eoArtist,
                              HTagLib.albumSetter <$> eoAlbum,
                              HTagLib.genreSetter <$> eoGenre,
                              toSetter HTagLib.yearSetter eoYear,
                              toSetter HTagLib.trackNumberSetter eoTrack
                            ]
                  HTagLib.setTags filename Nothing setter
              )
      Options.Check (Options.CheckOptions {..}) -> do
        -- Get the checks from the CLI and fallback to the config file
        checks <- maybe Config.readChecks pure coChecks
        Conduit.runConduitRes $
          fileOrDirectoryC coFilesOrDirectory
            .| Conduit.mapM_C
              ( \file -> do
                  let filename = Path.prjSomeBase Path.toFilePath file
                  track <- AudioTrack.getTags file
                  traverse_ (checkPrintError filename track) checks
              )
  where
    toSetter _ Nothing = Nothing
    toSetter setter (Just Options.Remove) = Just $ setter Nothing
    toSetter setter (Just (Options.Set v)) = Just . setter $ Just v
    checkPrintError file track check =
      whenJust (Check.check check track) $ \err ->
        putTextLn $ fromString file <> ": " <> err

exceptions :: SomeException -> IO ()
exceptions someException = do
  Text.hPutStr stderr message
  exitFailure
  where
    message
      | Just configException <- fromException someException =
          Config.render configException <> "\n"
      | otherwise = "Unknown exception: " <> show someException <> "\n"
