module Check.Artist
  ( Check (..),
    check,
    Error (..),
    errorToText,
  )
where

import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Model.Album qualified as Album
import Model.Artist qualified as Artist
import Model.AudioTrack qualified as AudioTrack
import Sound.HTagLib qualified as HTagLib
import Sound.HTagLib.Extra qualified as HTagLib
import "extra" Data.List.NonEmpty.Extra qualified as NonEmpty

newtype Check
  = SameGenre
      -- | Map from artist name to allowed multiple genres
      (Map Text (NonEmpty HTagLib.Genre))
  deriving (Eq, Show)

newtype Error = SameGenreError (NonEmpty HTagLib.Genre)
  deriving (Eq, Show)

errorToText :: Error -> Text
errorToText (SameGenreError genre) =
  "Tracks do not all have the same genre: "
    <> Text.intercalate ", " (HTagLib.unGenre <$> toList genre)

check :: Check -> Artist.Artist -> Either Error ()
check (SameGenre artistToAllowedGenres) artist
  | length genres == 1 = Right ()
  | Just allowedGenres <- mbAllowedGenres,
    all (`elem` allowedGenres) genres =
      Right ()
  | otherwise = Left $ SameGenreError genres
  where
    albums = Artist.albums artist
    genres =
      NonEmpty.nubOrd $ AudioTrack.atGenre <$> (Album.tracks =<< albums)
    albumArtistOrArtist = Artist.albumArtistOrArtist artist
    mbAllowedGenres =
      Map.lookup
        (HTagLib.unAlbumArtistOrArtist albumArtistOrArtist)
        artistToAllowedGenres
