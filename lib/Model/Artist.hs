module Model.Artist
  ( Artist,
    mkArtist,
    addAlbum,
    albumArtistOrArtist,
    albums,
  )
where

import Data.List.NonEmpty ((<|))
import Data.Text qualified as Text
import Model.Album qualified as Album
import Sound.HTagLib.Extra qualified as HTagLib

newtype Artist = Artist (NonEmpty Album.Album)
  deriving (Eq, Show)

mkArtist :: NonEmpty Album.Album -> Maybe Artist
mkArtist albums'@(firstAlbum :| otherAlbums)
  | allSameAlbumArtistOrArtist = Just $ Artist albums'
  | otherwise = Nothing
  where
    firstAlbumArtist = Album.albumArtist firstAlbum
    firstArtist = Album.artist firstAlbum
    haveAlbumArtist =
      not $ Text.null $ HTagLib.unAlbumArtist firstAlbumArtist
    allSameAlbumArtist =
      all ((== firstAlbumArtist) . Album.albumArtist) otherAlbums
    allSameArtist = all ((== firstArtist) . Album.artist) otherAlbums
    allSameAlbumArtistOrArtist =
      if haveAlbumArtist
        then allSameAlbumArtist && firstAlbumArtist /= "Various Artists"
        else allSameArtist

addAlbum :: Album.Album -> Artist -> Maybe Artist
addAlbum a (Artist albums') = mkArtist (a <| albums')

albumArtistOrArtist :: Artist -> HTagLib.AlbumArtistOrArtist
albumArtistOrArtist (Artist (a :| _)) = Album.albumArtistOrArtist a

albums :: Artist -> NonEmpty Album.Album
albums (Artist albums') = albums'
