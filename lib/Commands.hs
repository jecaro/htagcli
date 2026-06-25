module Commands
  ( getTags,
    setTags,
    checkTrack,
    checkDisc,
    checkArtist,
    FixFilePathsOptions (..),
    fixFilePaths,
    fixFilePaths',
    Error (..),
    errorToText,
  )
where

import Check.Artist qualified as Artist
import Check.Disc qualified as Disc
import Check.Track qualified as Track
import Model.Artist qualified as Artist
import Model.AudioTrack qualified as AudioTrack
import Model.Disc qualified as Disc
import Model.Pattern qualified as Pattern
import Model.SetTagsOptions qualified as SetTagsOptions
import Path ((</>))
import Path qualified
import Path.IO qualified as Path
import Path.IO.Extra qualified as Path
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
  SetTagsOptions.SetTagsOptions ->
  Path.Path Path.Abs Path.File ->
  m ()
setTags options filename =
  HTagLib.setTags
    (Path.toFilePath filename)
    Nothing
    (SetTagsOptions.setter options)

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
  { fiDryRun :: Bool,
    fiBaseDirectory :: Path.Path Path.Abs Path.Dir,
    fiFormatting :: Pattern.Formatting,
    fiPattern :: Pattern.Pattern,
    fiCoverImages :: Maybe (NonEmpty (Path.Path Path.Rel Path.File))
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
      whenM (Path.doesFileExist toFileAbs) $
        Exception.throwIO $
          TargetFileAlreadyExists toFileAbs

      unless fiDryRun $ do
        Path.ensureDir $ Path.parent toFileAbs

        Path.renameFile fromFile toFileAbs

        -- Move the cover if there
        let parentDir = Path.parent fromFile
        whenJust fiCoverImages $ \covers -> do
          forM_ covers $ \cover -> do
            whenM (Path.doesFileExist (parentDir </> cover)) $ do
              Path.renameFile
                (parentDir </> cover)
                (Path.parent toFileAbs </> cover)

        Path.removeDirAndParentsIfEmpty parentDir

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
