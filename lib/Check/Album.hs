module Check.Album
  ( Check (..),
    Error (..),
    errorToText,
    check,
  )
where

import Data.List qualified as List
import Data.Text qualified as Text
import Model.Album qualified as Album
import Model.Disc qualified as Disc
import Model.Tag qualified as Tag
import Sound.HTagLib.Extra qualified as HTagLib

data Check
  = DiscsSequential
  | SameTags (NonEmpty Tag.Tag)
  deriving (Eq, Show)

data Error
  = DiscsNotSequential
  | SameTagsError (NonEmpty Tag.Tag)
  deriving (Eq, Show)

errorToText :: Error -> Text
errorToText DiscsNotSequential =
  "Disc numbers are not sequentially numbered"
errorToText (SameTagsError tags) =
  "These tags are not the same for all discs in the album: "
    <> Text.intercalate ", " (Tag.asText <$> toList tags)

check :: Check -> Album.Album -> Either Error ()
check DiscsSequential album
  | length (Album.discs album) == 1 = Right ()
  | otherwise = case traverse Disc.disc (Album.discs album) of
      Nothing -> Left DiscsNotSequential
      Just numbers ->
        if sequential $ toList $ HTagLib.unDiscNumber <$> numbers
          then Right ()
          else Left DiscsNotSequential
  where
    sequential list = and $ zipWith (==) (List.sort list) [1 ..]
check (SameTags tagsToCheck) album = case checkedTags of
  [] -> Right ()
  (tag : tags) -> Left (SameTagsError (tag :| tags))
  where
    checkedTags = mapMaybe (Album.haveSameTag' album) (toList tagsToCheck)
