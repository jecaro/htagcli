module Check (Check (..), check) where

import AudioTrack (AudioTrack (..), haveTag)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text qualified as Text
import Sound.HTagLib (unGenre)
import Tag (Tag (..), render)

data Check
  = TagsExist (NonEmpty Tag)
  | GenreAmong (NonEmpty Text)
  deriving (Show)

check :: Check -> AudioTrack -> Maybe Text
check (TagsExist tags) track
  | null missingTags = Nothing
  | otherwise =
      Just $ "Missing tag(s): " <> Text.intercalate ", " (render <$> missingTags)
  where
    missingTags = NonEmpty.filter (not . (`haveTag` track)) tags
check (GenreAmong genres) track
  | unGenre (atGenre track) `elem` genres = Nothing
  | otherwise =
      Just $
        "Genre mismatch: expected one of "
          <> Text.intercalate ", " (NonEmpty.toList genres)
          <> ", got "
          <> unGenre (atGenre track)
