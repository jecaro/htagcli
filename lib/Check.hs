module Check (Check (..), check) where

import AudioTrack qualified
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text qualified as Text
import Sound.HTagLib qualified as HTagLib
import Tag qualified

data Check
  = TagsExist (NonEmpty Tag.Tag)
  | GenreAmong (NonEmpty Text)
  deriving (Show)

check :: Check -> AudioTrack.AudioTrack -> Maybe Text
check (TagsExist tags) track
  | null missingTags = Nothing
  | otherwise =
      Just $
        "Missing tag(s): "
          <> Text.intercalate ", " (Tag.render <$> missingTags)
  where
    missingTags = NonEmpty.filter (not . (`AudioTrack.haveTag` track)) tags
check (GenreAmong genres) track
  | HTagLib.unGenre (AudioTrack.atGenre track) `elem` genres = Nothing
  | otherwise =
      Just $
        "Genre mismatch: expected one of "
          <> Text.intercalate ", " (NonEmpty.toList genres)
          <> ", got "
          <> HTagLib.unGenre (AudioTrack.atGenre track)
