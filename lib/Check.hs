module Check (Check (..), check, render) where

import AudioTrack qualified
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text qualified as Text
import Path qualified
import Pattern qualified
import Sound.HTagLib qualified as HTagLib
import System.FilePath qualified as FilePath
import Tag qualified

data Check
  = TagsExist (NonEmpty Tag.Tag)
  | GenreAmong (NonEmpty Text)
  | FilenameMatches Pattern.Pattern
  deriving (Show)

data Error
  = MissingTags (NonEmpty Tag.Tag)
  | GenreMismatch (NonEmpty Text) Text
  | FilenameMismatch Text Text
  deriving (Show)

render :: Error -> Text
render (MissingTags tags) =
  "Missing tag(s): "
    <> Text.intercalate ", " (Tag.asText <$> NonEmpty.toList tags)
render (GenreMismatch expected genre) =
  "Genre mismatch: expected one of "
    <> Text.intercalate ", " (NonEmpty.toList expected)
    <> ", got "
    <> genre
render (FilenameMismatch expected actual) =
  "Filename does not match the expected pattern expected \""
    <> expected
    <> "\", got \""
    <> actual
    <> "\""

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
check (FilenameMatches pieces) track
  | isJust $ Text.stripSuffix expected actual = Right ()
  | otherwise = Left $ FilenameMismatch expected actual
  where
    expected = Pattern.format track pieces
    actual = toText $ FilePath.dropExtension filename
    filename = Path.prjSomeBase Path.toFilePath $ AudioTrack.atFile track
