module Check.Artist
  ( Check (..),
    check,
    Error (..),
    errorToText,
  )
where

import Model.Album qualified as Album
import Model.Artist qualified as Artist
import Model.AudioTrack qualified as AudioTrack

data Check = SameGenre
  deriving (Eq, Show)

data Error = SameGenreError
  deriving (Eq, Show)

errorToText :: Error -> Text
errorToText SameGenreError = "Tracks do not all have the same genre"

check :: Check -> Artist.Artist -> Either Error ()
check SameGenre artist
  | all (== firstGenre) otherGenres = Right ()
  | otherwise = Left SameGenreError
  where
    albums = Artist.albums artist
    (firstTrack :| otherTracks) = Album.tracks =<< albums
    firstGenre = AudioTrack.atGenre firstTrack
    otherGenres = AudioTrack.atGenre <$> otherTracks
