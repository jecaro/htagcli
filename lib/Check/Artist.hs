module Check.Artist
  ( Check (..),
    check,
    Error (..),
    errorToText,
  )
where

import Data.List.NonEmpty qualified as NonEmpty
import Data.Text qualified as Text
import Model.Album qualified as Album
import Model.Artist qualified as Artist
import Model.AudioTrack qualified as AudioTrack
import Sound.HTagLib qualified as HTagLib
import "extra" Data.List.NonEmpty.Extra qualified as NonEmpty

data Check = SameGenre
  deriving (Eq, Show)

newtype Error = SameGenreError (NonEmpty HTagLib.Genre)
  deriving (Eq, Show)

errorToText :: Error -> Text
errorToText (SameGenreError genre) =
  "Tracks do not all have the same genre: "
    <> Text.intercalate ", " (HTagLib.unGenre <$> toList genre)

check :: Check -> Artist.Artist -> Either Error ()
check SameGenre artist
  | length uniqueGenres == 1 = Right ()
  | otherwise = Left $ SameGenreError uniqueGenres
  where
    albums = Artist.albums artist
    genres = AudioTrack.atGenre <$> (Album.tracks =<< albums)
    uniqueGenres = NonEmpty.nubOrd $ NonEmpty.sort genres
