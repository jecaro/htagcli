module Artist
  ( Artist,
    mkArtist,
    addAlbum,
    artist,
    albums,
  )
where

import Album qualified
import Data.List.NonEmpty ((<|))
import Sound.HTagLib qualified as HTagLib

newtype Artist = Artist (NonEmpty Album.Album)
  deriving (Eq, Show)

mkArtist :: NonEmpty Album.Album -> Maybe Artist
mkArtist albums'@(firstAlbum :| otherAlbums)
  | (allSameAlbumArtist && (firstAlbumArtist /= "Various Artists"))
      || allSameArtist =
      Just $ Artist albums'
  | otherwise = Nothing
  where
    firstAlbumArtist = Album.albumArtist firstAlbum
    firstArtist = Album.artist firstAlbum
    allSameAlbumArtist =
      all
        ((== firstAlbumArtist) . Album.albumArtist)
        otherAlbums
    allSameArtist = all ((== firstArtist) . Album.artist) otherAlbums

addAlbum :: Album.Album -> Artist -> Maybe Artist
addAlbum album (Artist albums') = mkArtist (album <| albums')

artist :: Artist -> HTagLib.Artist
artist (Artist (album :| _)) = Album.artist album

albums :: Artist -> NonEmpty Album.Album
albums (Artist albums') = albums'
