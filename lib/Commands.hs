module Commands
  ( getTags,
    setTags,
    checkTrack,
    checkAlbum,
    checkArtist,
    FixFilePathsOptions (..),
    fixFilePaths,
    fixFilePaths',
    errorToText,
  )
where

import Check.Album qualified as Album
import Check.Artist qualified as Artist
import Check.Track qualified as Track
import Model.Album qualified as Album
import Model.Artist qualified as Artist
import Model.AudioTrack qualified as AudioTrack
import Model.Pattern qualified as Pattern
import Model.SetTagsOptions qualified as SetTagsOptions
import Path ((</>))
import Path qualified
import Path.IO qualified as Path
import Path.IO.Extra qualified as Path
import Sound.HTagLib qualified as HTagLib
import Sound.HTagLib.Extra qualified as HTagLib
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

checkTrack :: (MonadIO m) => [Track.Check] -> AudioTrack.AudioTrack -> m ()
checkTrack checks track = do
  traverse_ (checkPrintError track) checks
  where
    checkPrintError track' check =
      whenLeft_ (Track.check check track') $ \err ->
        putTextLn $
          "File "
            <> fromString (Path.toFilePath file)
            <> ": "
            <> Track.errorToText err
    file = AudioTrack.atFile track

checkAlbum ::
  (MonadIO m) => [Album.Check] -> Album.Album -> m ()
checkAlbum checks album = traverse_ checkPrintError checks
  where
    checkPrintError check =
      whenLeftM_ (Album.check check album) $ \err ->
        putTextLn $
          "Album "
            <> artistOrAlbumArtistTxt
            <> albumTxt
            <> discTxt
            <> ": "
            <> Album.errorToText err
    artistOrAlbumArtistTxt = Album.albumArtistOrArtist album
    albumTxt = "/" <> HTagLib.unAlbum (Album.album album)
    discTxt
      | Just disc <- Album.disc album =
          "/Disc " <> show (HTagLib.unDiscNumber disc)
      | otherwise = ""

checkArtist ::
  (MonadIO m) =>
  Maybe Artist.Check ->
  Artist.Artist ->
  m ()
checkArtist Nothing _ = pure ()
checkArtist (Just check) artist = do
  whenLeft_ (Artist.check check artist) $ \err -> do
    putTextLn $
      "Artist "
        <> HTagLib.unArtist (Artist.artist artist)
        <> ": "
        <> Artist.errorToText err

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
