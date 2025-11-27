module Check.Track
  ( Check (..),
    check,
    Error (..),
    errorToText,
  )
where

import Data.List.NonEmpty qualified as NonEmpty
import Data.Text qualified as Text
import Model.AudioTrack qualified as AudioTrack
import Model.Pattern qualified as Pattern
import Model.Tag qualified as Tag
import Path qualified
import Sound.HTagLib qualified as HTagLib

data Check
  = TagsExist (NonEmpty Tag.Tag)
  | GenreAmong (NonEmpty Text)
  | FilenameMatches Pattern.Pattern Pattern.Formatting
  deriving (Show)

data Error
  = MissingTags (NonEmpty Tag.Tag)
  | GenreMismatch (NonEmpty Text) Text
  | FilenameMismatch Text
  deriving (Eq, Show)

errorToText :: Error -> Text
errorToText (MissingTags tags) =
  "Missing tag(s): "
    <> Text.intercalate ", " (Tag.asText <$> NonEmpty.toList tags)
errorToText (GenreMismatch expected genre) =
  "Genre mismatch: expected one of "
    <> Text.intercalate ", " (NonEmpty.toList expected)
    <> ", got "
    <> genre
errorToText (FilenameMismatch expected) =
  "Filename does not match the pattern, expected \"" <> expected <> "\""

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
          genres
          (HTagLib.unGenre $ AudioTrack.atGenre track)
check (FilenameMatches pattern formatting) track = do
  whenJust (nonEmpty $ Pattern.tags pattern) $ \tags ->
    check (TagsExist tags) track
  if Pattern.match formatting track pattern filename
    then Right ()
    else Left $ FilenameMismatch expected
  where
    filename = Path.toFilePath $ AudioTrack.atFile track
    expected = Pattern.format formatting track pattern
