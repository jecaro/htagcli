module Check.Artist
  ( Check (..),
    check,
    Error (..),
    errorToText,
  )
where

import AudioTrack qualified

data Check = SameGenre
  deriving (Eq, Show)

data Error = SameGenreError
  deriving (Eq, Show)

errorToText :: Error -> Text
errorToText SameGenreError = "Tracks do not all have the same genre"

check :: Check -> NonEmpty (NonEmpty AudioTrack.AudioTrack) -> Either Error ()
check SameGenre albums
  | all (== head genres) genres = Right ()
  | otherwise = Left SameGenreError
  where
    tracks = join albums
    genres = AudioTrack.atGenre <$> tracks
