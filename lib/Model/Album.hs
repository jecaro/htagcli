module Model.Album
  ( Album,
    mkAlbum,
    addDisc,
    discs,
    album,
    artist,
    albumArtist,
    albumArtistOrArtist,
  )
where

import Data.List.NonEmpty ((<|))
import Model.Disc qualified as Disc
import Sound.HTagLib qualified as HTagLib
import Sound.HTagLib.Extra qualified as HTagLib

newtype Album = Album (NonEmpty Disc.Disc)
  deriving (Eq, Show)

mkAlbum :: NonEmpty Disc.Disc -> Maybe Album
mkAlbum discs'@(firstDisc :| otherDiscs)
  | allSameAlbum
      && (allSameAlbumArtist || allSameArtist) =
      Just $ Album discs'
  | otherwise = Nothing
  where
    firstAlbum = Disc.album firstDisc
    firstAlbumArtist = Disc.albumArtist firstDisc
    firstArtist = Disc.artist firstDisc
    allSameAlbum = all ((== firstAlbum) . Disc.album) otherDiscs
    allSameAlbumArtist = all ((== firstAlbumArtist) . Disc.albumArtist) otherDiscs
    allSameArtist = all ((== firstArtist) . Disc.artist) otherDiscs

addDisc :: Disc.Disc -> Album -> Maybe Album
addDisc d (Album discs') = mkAlbum (d <| discs')

discs :: Album -> NonEmpty Disc.Disc
discs (Album discs') = discs'

album :: Album -> HTagLib.Album
album (Album (d :| _)) = Disc.album d

artist :: Album -> HTagLib.Artist
artist (Album (d :| _)) = Disc.artist d

albumArtist :: Album -> HTagLib.AlbumArtist
albumArtist (Album (d :| _)) = Disc.albumArtist d

albumArtistOrArtist :: Album -> HTagLib.AlbumArtistOrArtist
albumArtistOrArtist (Album (d :| _)) = Disc.albumArtistOrArtist d
