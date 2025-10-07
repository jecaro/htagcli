module Check (Check (..), check, render) where

import AudioTrack qualified
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text qualified as Text
import Sound.HTagLib qualified as HTagLib
import Tag qualified

data Check
  = TagsExist (NonEmpty Tag.Tag)
  | GenreAmong (NonEmpty Text)
  deriving (Show)

data Error
  = MissingTags (NonEmpty Tag.Tag)
  | GenreMismatch Text (NonEmpty Text)
  deriving (Show)

render :: Error -> Text
render (MissingTags tags) =
  "Missing tag(s): "
    <> Text.intercalate ", " (Tag.render <$> NonEmpty.toList tags)
render (GenreMismatch genre expected) =
  "Genre mismatch: expected one of "
    <> Text.intercalate ", " (NonEmpty.toList expected)
    <> ", got "
    <> genre

check :: Check -> AudioTrack.AudioTrack -> Either Error ()
check (TagsExist tags) track =
  case missingTags of
    [] -> Right ()
    (x : xs) -> Left $ MissingTags $ x :| xs
  where
    missingTags = NonEmpty.filter (not . (`AudioTrack.haveTag` track)) tags
check (GenreAmong genres) track
  | HTagLib.unGenre (AudioTrack.atGenre track) `elem` genres = Right ()
  | otherwise =
      Left $
        GenreMismatch
          (HTagLib.unGenre $ AudioTrack.atGenre track)
          genres
