module Check.Album
  ( Check (..),
    check,
    Error (..),
    errorToText,
  )
where

import Data.Text qualified as Text
import Model.Album qualified as Album
import Model.AudioTrack qualified as AudioTrack
import Model.Tag qualified as Tag
import Path ((</>))
import Path qualified
import Path.IO qualified as Path
import Sound.HTagLib qualified as HTagLib

data Check
  = HaveCover (NonEmpty (Path.Path Path.Rel Path.File))
  | InSameDir
  | SameTags (NonEmpty Tag.Tag)
  | TracksSequential
  deriving (Eq, Show)

data Error
  = NotInSameDir
  | MissingCover (Path.Path Path.Abs Path.Dir)
  | SameTagsError (NonEmpty Tag.Tag)
  | TracksNotSequential
  deriving (Eq, Show)

errorToText :: Error -> Text
errorToText NotInSameDir =
  "Audio tracks are not all in the same directory"
errorToText (MissingCover directory) =
  "Missing cover in directory: " <> Text.pack (Path.toFilePath directory)
errorToText (SameTagsError tags) =
  "These tags are not the same for all tracks in the album: "
    <> Text.intercalate ", " (Tag.asText <$> toList tags)
errorToText TracksNotSequential = "Tracks are not sequentially numbered"

check ::
  (MonadIO m) =>
  Check ->
  Album.Album ->
  m (Either Error ())
check InSameDir album
  | isJust $ Album.directory album = pure $ Right ()
  | otherwise = pure $ Left NotInSameDir
check (HaveCover coverFilenames) album
  | Just dir <- Album.directory album = do
      let absFiles = (dir </>) <$> coverFilenames
      ifM
        (anyM Path.doesFileExist absFiles)
        (pure $ Right ())
        (pure $ Left (MissingCover dir))
  | otherwise = pure $ Left NotInSameDir
check (SameTags tagsToCheck) album = pure $ case checkedTags of
  [] -> Right ()
  (tag : tags) -> Left (SameTagsError (tag :| tags))
  where
    checkedTags = mapMaybe (Album.haveSameTag' album) (toList tagsToCheck)
check TracksSequential album = pure $ case mbNumbers of
  Nothing -> Left TracksNotSequential
  Just numbers ->
    if sequential $ toList $ HTagLib.unTrackNumber <$> numbers
      then Right ()
      else Left TracksNotSequential
  where
    mbNumbers = traverse AudioTrack.atTrack (Album.tracks album)
    sequential list = and $ zipWith (==) (sort list) [1 ..]
