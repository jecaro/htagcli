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
import Tag qualified
import "extra" Data.List.NonEmpty.Extra qualified as NonEmpty

data Check
  = HaveCover (NonEmpty (Path.Path Path.Rel Path.File))
  | InSameDir
  | SameTag (NonEmpty Tag.Tag)
  deriving (Eq, Show)

data Error
  = NotInSameDir
  | MissingCover (Path.Path Path.Abs Path.Dir)
  | SameTagError (NonEmpty Tag.Tag)
  deriving (Eq, Show)

errorToText :: Error -> Text
errorToText NotInSameDir =
  "Audio tracks are not all in the same directory"
errorToText (MissingCover directory) =
  "Missing cover in directory: " <> Text.pack (Path.toFilePath directory)
errorToText (SameTagError tags) =
  "These tags are not the same for all tracks in the album: "
    <> Text.intercalate ", " (Tag.asText <$> NonEmpty.toList tags)

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
check (SameTag tagsToCheck) tracks = pure $ case checkedTags of
  [] -> Right ()
  (tag : tags) -> Left (SameTagError (tag :| tags))
  where
    checkedTags = mapMaybe (haveSameTag' tracks) (toList tagsToCheck)

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
