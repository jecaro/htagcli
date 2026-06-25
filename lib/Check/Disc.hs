module Check.Disc
  ( Check (..),
    Cover (..),
    Size (..),
    check,
    Error (..),
    errorToText,
  )
where

import Codec.Picture qualified as Picture
import Control.Monad.Extra qualified as Monad
import Control.Monad.Trans.Except qualified as Except
import Data.Text qualified as Text
import Model.AudioTrack qualified as AudioTrack
import Model.Cover as Cover
import Model.Disc qualified as Disc
import Model.Tag qualified as Tag
import Path ((</>))
import Path qualified
import Path.IO qualified as Path
import Sound.HTagLib qualified as HTagLib

data Check
  = HaveCover Cover.Cover
  | InSameDir
  | SameTags (NonEmpty Tag.Tag)
  | TracksSequential
  deriving (Eq, Show)

data Error
  = NotInSameDir
  | MissingCover (Path.Path Path.Abs Path.Dir)
  | BadCoverSize (Path.Path Path.Abs Path.File) Size
  | UnableToReadCover (Path.Path Path.Abs Path.File) Text
  | SameTagsError (NonEmpty Tag.Tag)
  | TracksNotSequential
  deriving (Eq, Show)

errorToText :: Error -> Text
errorToText NotInSameDir =
  "Audio tracks are not all in the same directory"
errorToText (MissingCover directory) =
  "Missing cover in directory: " <> Text.pack (Path.toFilePath directory)
errorToText (BadCoverSize file size) =
  "Cover file "
    <> toText (Path.toFilePath file)
    <> " has size out of range: "
    <> Cover.sizeToText size
errorToText (UnableToReadCover file err) =
  "Unable to read cover file "
    <> toText (Path.toFilePath file)
    <> ": "
    <> err
errorToText (SameTagsError tags) =
  "These tags are not the same for all tracks in the disc: "
    <> Text.intercalate ", " (Tag.asText <$> toList tags)
errorToText TracksNotSequential = "Tracks are not sequentially numbered"

check ::
  (MonadIO m) =>
  Check ->
  Disc.Disc ->
  m (Either Error ())
check InSameDir d
  | isJust $ Disc.directory d = pure $ Right ()
  | otherwise = pure $ Left NotInSameDir
check (HaveCover cover@Cover {..}) d
  | Just dir <- Disc.directory d = runExceptT $ do
      let absFiles = (dir </>) <$> coPaths

      coverFile <-
        maybeToExceptT (MissingCover dir) $
          MaybeT $
            Monad.findM Path.doesFileExist (toList absFiles)
      -- Reading the image is very slow, so only do it if we have size
      -- constraints
      when (Cover.haveRange cover) $ do
        picture <-
          Except.withExceptT (UnableToReadCover coverFile . toText) $
            ExceptT $
              readImage coverFile

        let size = Cover.pictureSize picture
        unless (Cover.withinRange cover size) $
          Except.throwE $
            BadCoverSize coverFile size
  | otherwise = pure $ Left NotInSameDir
  where
    readImage = liftIO . Picture.readImage . Path.toFilePath
check (SameTags tagsToCheck) d = pure $ case checkedTags of
  [] -> Right ()
  (tag : tags) -> Left (SameTagsError (tag :| tags))
  where
    checkedTags = mapMaybe (Disc.haveSameTag' d) (toList tagsToCheck)
check TracksSequential d = pure $ case mbNumbers of
  Nothing -> Left TracksNotSequential
  Just numbers ->
    if sequential $ toList $ HTagLib.unTrackNumber <$> numbers
      then Right ()
      else Left TracksNotSequential
  where
    mbNumbers = traverse AudioTrack.atTrack (Disc.tracks d)
    sequential list = and $ zipWith (==) (sort list) [1 ..]
