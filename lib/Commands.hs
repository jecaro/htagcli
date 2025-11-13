module Commands
  ( getTags,
    setTags,
    checkFile,
    checkAlbum,
    FixFilePathsOptions (..),
    fixFilePaths,
    fixFilePaths',
    errorToText,
  )
where

import AudioTrack qualified
import Check.Album qualified as Album
import Check.File qualified as File
import Data.List.NonEmpty qualified as NonEmpty
import Path ((</>))
import Path qualified
import Path.IO qualified as Path
import Path.IO.Extra qualified as Path
import Pattern qualified
import SetTagsOptions qualified
import Sound.HTagLib qualified as HTagLib
import UnliftIO.Exception qualified as Exception

newtype Error = UnableToFormatFile (Path.Path Path.Abs Path.File)
  deriving (Show)

instance Exception.Exception Error

errorToText :: Error -> Text
errorToText (UnableToFormatFile file) = "Unable to format file: " <> show file

getTags :: (MonadIO m) => Path.Path Path.Abs Path.File -> m ()
getTags = putTextLn . AudioTrack.asText <=< AudioTrack.getTags

setTags ::
  (MonadIO m) =>
  SetTagsOptions.SetTagsOptions ->
  Path.Path Path.Abs Path.File ->
  m ()
setTags options filename =
  HTagLib.setTags
    (Path.toFilePath filename)
    Nothing
    (SetTagsOptions.setter options)

checkFile :: (MonadIO m) => [File.Check] -> AudioTrack.AudioTrack -> m ()
checkFile checks track = do
  traverse_ (checkPrintError track) checks
  where
    checkPrintError track' check =
      whenLeft_ (File.check check track') $ \err ->
        putTextLn $
          "File "
            <> fromString (Path.toFilePath file)
            <> ": "
            <> File.errorToText err
    file = AudioTrack.atFile track

checkAlbum ::
  (MonadIO m) => [Album.Check] -> NonEmpty AudioTrack.AudioTrack -> m ()
checkAlbum checks tracks = do
  traverse_ (checkPrintError tracks) checks
  where
    checkPrintError tracks' check =
      whenLeftM_ (Album.check check tracks') $ \err ->
        putTextLn $
          "Album " <> HTagLib.unAlbum album <> ": " <> Album.errorToText err
    album = AudioTrack.atAlbum $ NonEmpty.head tracks

data FixFilePathsOptions = FixFilePathsOptions
  { fiDryRun :: Bool,
    fiBaseDirectory :: Path.Path Path.Abs Path.Dir,
    fiFormatting :: Pattern.Formatting,
    fiPattern :: Pattern.Pattern
  }
  deriving (Show)

-- | Version for testing that returns the new path if changed
fixFilePaths' ::
  (MonadIO m) =>
  FixFilePathsOptions ->
  Path.Path Path.Abs Path.File ->
  m (Maybe (Path.Path Path.Abs Path.File))
fixFilePaths' FixFilePathsOptions {..} fromFile = do
  track <- AudioTrack.getTags fromFile
  toFile <-
    Exception.fromEither $
      maybeToRight (UnableToFormatFile fromFile) $
        Pattern.toPath fiFormatting track fiPattern
  let toFileAbs = fiBaseDirectory </> toFile
  if toFileAbs == fromFile
    then pure Nothing
    else do
      unless fiDryRun $ do
        Path.ensureDir $ Path.parent toFileAbs
        Path.renameFile fromFile toFileAbs
        Path.removeDirAndParentsIfEmpty $ Path.parent fromFile

      pure (Just toFileAbs)

fixFilePaths ::
  (MonadIO m) =>
  FixFilePathsOptions ->
  Path.Path Path.Abs Path.File ->
  m ()
fixFilePaths options fromFile = do
  mbNewPath <- fixFilePaths' options fromFile
  whenJust mbNewPath $ \toFile ->
    putTextLn $
      fromString (Path.toFilePath fromFile)
        <> " -> "
        <> fromString (Path.toFilePath toFile)
