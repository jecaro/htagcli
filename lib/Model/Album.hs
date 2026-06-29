module Model.Album
  ( Album,
    mkAlbum,
    addDisc,
    discs,
    years,
    album,
    artist,
    albumArtist,
    albumArtistOrArtist,
    haveSameTag',
  )
where

import Data.List.Extra qualified as List
import Data.List.NonEmpty ((<|))
import Data.Text qualified as Text
import Model.Disc qualified as Disc
import Model.Tag qualified as Tag
import Sound.HTagLib qualified as HTagLib
import Sound.HTagLib.Extra qualified as HTagLib
import "extra" Data.List.NonEmpty.Extra qualified as NonEmpty

newtype Album = Album (NonEmpty Disc.Disc)
  deriving (Eq, Show)

mkAlbum :: NonEmpty Disc.Disc -> Maybe Album
mkAlbum discs'@(firstDisc :| otherDiscs)
  | allSameAlbum && allSameAlbumArtistOrArtist =
      Just $
        Album $
          NonEmpty.sortOn (fmap HTagLib.unDiscNumber . Disc.disc) discs'
  | otherwise = Nothing
  where
    firstAlbum = Disc.album firstDisc
    firstAlbumArtist = Disc.albumArtist firstDisc
    firstArtist = Disc.artist firstDisc
    haveAlbumArtist = not $ Text.null $ HTagLib.unAlbumArtist firstAlbumArtist
    allSameAlbum = all ((== firstAlbum) . Disc.album) otherDiscs
    allSameAlbumArtist = all ((== firstAlbumArtist) . Disc.albumArtist) otherDiscs
    allSameArtist = all ((== firstArtist) . Disc.artist) otherDiscs
    allSameAlbumArtistOrArtist =
      if haveAlbumArtist
        then allSameAlbumArtist
        else allSameArtist

addDisc :: Disc.Disc -> Album -> Maybe Album
addDisc d (Album discs') = mkAlbum (d <| discs')

discs :: Album -> NonEmpty Disc.Disc
discs (Album discs') = discs'

years :: Album -> [HTagLib.Year]
years (Album discs') = List.nubSort $ foldMap Disc.years $ toList discs'

album :: Album -> HTagLib.Album
album (Album (d :| _)) = Disc.album d

artist :: Album -> HTagLib.Artist
artist (Album (d :| _)) = Disc.artist d

albumArtist :: Album -> HTagLib.AlbumArtist
albumArtist (Album (d :| _)) = Disc.albumArtist d

albumArtistOrArtist :: Album -> HTagLib.AlbumArtistOrArtist
albumArtistOrArtist (Album (d :| _)) = Disc.albumArtistOrArtist d

haveSameTag' :: Album -> Tag.Tag -> Maybe Tag.Tag
haveSameTag' a = guarded (not . haveSameTag a)

haveSameTag :: Album -> Tag.Tag -> Bool
haveSameTag (Album discs') tag = all (`Disc.haveSameTag` tag) discs'
