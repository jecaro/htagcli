module Check.Album
  ( Check (..),
    check,
    Error (..),
    errorToText,
  )
where

import AudioTrack qualified
import Data.Text qualified as Text
import Path ((</>))
import Path qualified
import Path.IO qualified as Path
import Sound.HTagLib qualified as HTagLib
import Tag qualified
import "extra" Data.List.NonEmpty.Extra qualified as NonEmpty

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
    <> Text.intercalate ", " (Tag.asText <$> NonEmpty.toList tags)
errorToText TracksNotSequential = "Tracks are not sequentially numbered"

getDirectories ::
  NonEmpty AudioTrack.AudioTrack -> NonEmpty (Path.Path Path.Abs Path.Dir)
getDirectories tracks =
  NonEmpty.nubOrd $ Path.parent . AudioTrack.atFile <$> tracks

check ::
  (MonadIO m) =>
  Check ->
  NonEmpty AudioTrack.AudioTrack ->
  m (Either Error ())
check InSameDir tracks
  | length (getDirectories tracks) == 1 = pure $ Right ()
  | otherwise = pure $ Left NotInSameDir
check (HaveCover coverFilenames) tracks
  | dir :| [] <- getDirectories tracks = do
      let absFiles = (dir </>) <$> coverFilenames
      ifM
        (anyM Path.doesFileExist absFiles)
        (pure $ Right ())
        (pure $ Left (MissingCover dir))
  | otherwise = pure $ Left NotInSameDir
check (SameTags tagsToCheck) tracks = pure $ case checkedTags of
  [] -> Right ()
  (tag : tags) -> Left (SameTagsError (tag :| tags))
  where
    checkedTags = mapMaybe (haveSameTag' tracks) (toList tagsToCheck)
check TracksSequential tracks = pure $ case mbNumbers of
  Nothing -> Left TracksNotSequential
  Just numbers ->
    if sequential $ toList $ HTagLib.unTrackNumber <$> numbers
      then Right ()
      else Left TracksNotSequential
  where
    mbNumbers = traverse AudioTrack.atTrack tracks
    sequential list = and $ zipWith (==) (sort list) [1 ..]

haveSameTag' :: NonEmpty AudioTrack.AudioTrack -> Tag.Tag -> Maybe Tag.Tag
haveSameTag' tracks = guarded (not . haveSameTag tracks)

haveSameTag :: NonEmpty AudioTrack.AudioTrack -> Tag.Tag -> Bool
haveSameTag tracks Tag.Title = haveSameTagWithGetter AudioTrack.atTitle tracks
haveSameTag tracks Tag.Artist = haveSameTagWithGetter AudioTrack.atArtist tracks
haveSameTag tracks Tag.AlbumArtist =
  haveSameTagWithGetter AudioTrack.atAlbumArtist tracks
haveSameTag tracks Tag.Album = haveSameTagWithGetter AudioTrack.atAlbum tracks
haveSameTag tracks Tag.Genre = haveSameTagWithGetter AudioTrack.atGenre tracks
haveSameTag tracks Tag.Year = haveSameTagWithGetter AudioTrack.atYear tracks
haveSameTag tracks Tag.Track = haveSameTagWithGetter AudioTrack.atTrack tracks
haveSameTag tracks Tag.Disc = haveSameTagWithGetter AudioTrack.atDisc tracks

haveSameTagWithGetter ::
  (Eq a) =>
  (AudioTrack.AudioTrack -> a) ->
  NonEmpty AudioTrack.AudioTrack ->
  Bool
haveSameTagWithGetter getTagValue tracks = all (== head tags) (tail tags)
  where
    tags = getTagValue <$> tracks
