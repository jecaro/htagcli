module Model.Artist
  ( Artist,
    mkArtist,
    addDisc,
    albumArtistOrArtist,
    discs,
  )
where

import Data.List.NonEmpty ((<|))
import Data.Text qualified as Text
import Model.Disc qualified as Disc
import Sound.HTagLib.Extra qualified as HTagLib

newtype Artist = Artist (NonEmpty Disc.Disc)
  deriving (Eq, Show)

mkArtist :: NonEmpty Disc.Disc -> Maybe Artist
mkArtist discs'@(firstDisc :| otherDiscs)
  | ( allSameAlbumArtist
        && not (Text.null $ HTagLib.unAlbumArtist firstAlbumArtist)
        && (firstAlbumArtist /= "Various Artists")
    )
      || allSameArtist =
      Just $ Artist discs'
  | otherwise = Nothing
  where
    firstAlbumArtist = Disc.albumArtist firstDisc
    firstArtist = Disc.artist firstDisc
    allSameAlbumArtist =
      all ((== firstAlbumArtist) . Disc.albumArtist) otherDiscs
    allSameArtist = all ((== firstArtist) . Disc.artist) otherDiscs

addDisc :: Disc.Disc -> Artist -> Maybe Artist
addDisc d (Artist discs') = mkArtist (d <| discs')

albumArtistOrArtist :: Artist -> HTagLib.AlbumArtistOrArtist
albumArtistOrArtist (Artist (d :| _)) = Disc.albumArtistOrArtist d

discs :: Artist -> NonEmpty Disc.Disc
discs (Artist discs') = discs'
