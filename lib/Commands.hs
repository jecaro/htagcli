module Commands
  ( getTags,
    setTags,
    checkTrack,
    checkDisc,
    checkAlbum,
    checkArtist,
    FixFilePathsOptions (..),
    fixFilePaths,
    Error (..),
    errorToText,
  )
where

import Check.Album qualified as Album
import Check.Artist qualified as Artist
import Check.Disc qualified as Disc
import Check.Track qualified as Track
import Commands.FileSystem qualified as FileSystem
import Model.Album qualified as Album
import Model.Artist qualified as Artist
import Model.AudioTrack qualified as AudioTrack
import Model.Disc qualified as Disc
import Model.Pattern qualified as Pattern
import Model.SetTags qualified as SetTags
import Path ((</>))
import Path qualified
import Sound.HTagLib qualified as HTagLib
import Sound.HTagLib.Extra qualified as HTagLib
import UnliftIO.Exception qualified as Exception

data Error
  = UnableToFormatFile (Path.Path Path.Abs Path.File)
  | TargetFileAlreadyExists (Path.Path Path.Abs Path.File)
  deriving (Show, Eq)

instance Exception.Exception Error

errorToText :: Error -> Text
errorToText (UnableToFormatFile file) = "Unable to format file: " <> show file
errorToText (TargetFileAlreadyExists file) = "Target file already exists: " <> show file

getTags :: (MonadIO m) => Path.Path Path.Abs Path.File -> m ()
getTags = putTextLn . AudioTrack.asText <=< AudioTrack.getTags

setTags ::
  (MonadIO m) =>
  SetTags.SetTags ->
  Path.Path Path.Abs Path.File ->
  m ()
setTags options filename =
  HTagLib.setTags
    (Path.toFilePath filename)
    Nothing
    (SetTags.setter options)

countTrues :: [Bool] -> Int
countTrues = length . filter id

checkTrack :: (MonadIO m) => [Track.Check] -> AudioTrack.AudioTrack -> m Int
checkTrack checks track = countTrues <$> traverse checkPrintError checks
  where
    checkPrintError check = do
      let result = Track.check check track
      whenLeft_ result $ \err ->
        putTextLn $
          "File "
            <> fromString (Path.toFilePath file)
            <> ": "
            <> Track.errorToText err
      pure $ isLeft result
    file = AudioTrack.atFile track

checkDisc :: (MonadIO m) => [Disc.Check] -> Disc.Disc -> m Int
checkDisc checks d = countTrues <$> traverse checkPrintError checks
  where
    checkPrintError check = do
      result <- Disc.check check d
      whenLeft_ result $ \err ->
        putTextLn $
          "Disc "
            <> albumArtistOrArtistTxt
            <> albumTxt
            <> discTxt
            <> ": "
            <> Disc.errorToText err
      pure $ isLeft result
    albumArtistOrArtistTxt =
      HTagLib.unAlbumArtistOrArtist $ Disc.albumArtistOrArtist d
    albumTxt = "/" <> HTagLib.unAlbum (Disc.album d)
    discTxt
      | Just discNum <- Disc.disc d =
          "/Disc " <> show (HTagLib.unDiscNumber discNum)
      | otherwise = ""

checkAlbum :: (MonadIO m) => [Album.Check] -> Album.Album -> m Int
checkAlbum checks a = countTrues <$> traverse checkPrintError checks
  where
    checkPrintError check = do
      let result = Album.check check a
      whenLeft_ result $ \err ->
        putTextLn $
          "Album "
            <> albumArtistOrArtistTxt
            <> albumTxt
            <> ": "
            <> Album.errorToText err
      pure $ isLeft result
    albumArtistOrArtistTxt =
      HTagLib.unAlbumArtistOrArtist $ Album.albumArtistOrArtist a
    albumTxt = "/" <> HTagLib.unAlbum (Album.album a)

checkArtist ::
  (MonadIO m) =>
  Maybe Artist.Check ->
  Artist.Artist ->
  m Bool
checkArtist Nothing _ = pure False
checkArtist (Just check) artist = do
  let result = Artist.check check artist
  whenLeft_ result $ \err -> do
    putTextLn $
      "Artist "
        <> albumArtistOrArtistTxt
        <> ": "
        <> Artist.errorToText err
  pure $ isLeft result
  where
    albumArtistOrArtistTxt =
      HTagLib.unAlbumArtistOrArtist $ Artist.albumArtistOrArtist artist

data FixFilePathsOptions = FixFilePathsOptions
  { fiBaseDirectory :: Path.Path Path.Abs Path.Dir,
    fiFormatting :: Pattern.Formatting,
    fiPattern :: Pattern.Pattern,
    fiCoverImages :: Maybe (NonEmpty (Path.Path Path.Rel Path.File))
  }
  deriving (Show)

fixFilePaths ::
  (MonadIO m) =>
  FileSystem.FileSystem m ->
  FixFilePathsOptions ->
  Path.Path Path.Abs Path.File ->
  m ()
fixFilePaths
  fileSystem@FileSystem.FileSystem {..}
  FixFilePathsOptions {..}
  fromFile = do
    track <- AudioTrack.getTags fromFile
    toFile <-
      Exception.fromEither $
        maybeToRight (UnableToFormatFile fromFile) $
          Pattern.toPath fiFormatting track fiPattern
    let toFileAbs = fiBaseDirectory </> toFile
    unless (toFileAbs == fromFile) $ do
      whenM (fiDoesFileExist toFileAbs) $
        Exception.throwIO $
          TargetFileAlreadyExists toFileAbs

      fiEnsureDir $ Path.parent toFileAbs
      putTextLn $
        fromString (Path.toFilePath fromFile)
          <> " -> "
          <> fromString (Path.toFilePath toFileAbs)
      fiRenameFile fromFile toFileAbs

      let parentDir = Path.parent fromFile
      whenJust fiCoverImages $ \covers ->
        forM_ covers $ \cover ->
          whenM (fiDoesFileExist (parentDir </> cover)) $ do
            let coverFrom = parentDir </> cover
                coverTo = Path.parent toFileAbs </> cover
            putTextLn $
              fromString (Path.toFilePath coverFrom)
                <> " -> "
                <> fromString (Path.toFilePath coverTo)
            fiRenameFile coverFrom coverTo

      FileSystem.removeDirAndParentsIfEmpty fileSystem parentDir
