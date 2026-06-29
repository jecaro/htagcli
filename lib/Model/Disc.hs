module Model.Disc
  ( Disc,
    mkDisc,
    addTrack,
    tracks,
    years,
    genres,
    artist,
    album,
    albumArtist,
    albumArtistOrArtist,
    disc,
    directory,
    haveSameTag,
    haveSameTag',
  )
where

import Data.List.Extra qualified as List
import Data.List.NonEmpty ((<|))
import Model.AudioTrack qualified as AudioTrack
import Model.Tag qualified as Tag
import Path qualified
import Sound.HTagLib qualified as HTagLib
import Sound.HTagLib.Extra qualified as HTagLib
import "extra" Data.List.NonEmpty.Extra qualified as NonEmpty

newtype Disc = Disc (NonEmpty AudioTrack.AudioTrack)
  deriving (Eq, Show)

mkDisc :: NonEmpty AudioTrack.AudioTrack -> Maybe Disc
mkDisc tracks'@(firstTrack :| otherTracks)
  | allSameAlbum && allSameDisc && allSameAlbumArtistOrArtist =
      Just $
        Disc $
          NonEmpty.sortOn
            (fmap HTagLib.unTrackNumber . AudioTrack.atTrack)
            tracks'
  | otherwise = Nothing
  where
    firstAlbum = AudioTrack.atAlbum firstTrack
    firstDisc = AudioTrack.atDisc firstTrack
    firstAlbumArtist = AudioTrack.atAlbumArtist firstTrack
    firstArtist = AudioTrack.atArtist firstTrack
    allSameAlbum = all ((== firstAlbum) . AudioTrack.atAlbum) otherTracks
    allSameDisc = all ((== firstDisc) . AudioTrack.atDisc) otherTracks
    allSameAlbumArtist =
      all ((== firstAlbumArtist) . AudioTrack.atAlbumArtist) otherTracks
    allSameArtist = all ((== firstArtist) . AudioTrack.atArtist) otherTracks
    allSameAlbumArtistOrArtist =
      if AudioTrack.haveTag Tag.AlbumArtist firstTrack
        then allSameAlbumArtist
        else allSameArtist

addTrack :: AudioTrack.AudioTrack -> Disc -> Maybe Disc
addTrack track (Disc tracks') = mkDisc (track <| tracks')

tracks :: Disc -> NonEmpty AudioTrack.AudioTrack
tracks (Disc tracks') = tracks'

years :: Disc -> [HTagLib.Year]
years (Disc tracks') = List.nubSort $ mapMaybe AudioTrack.atYear $ toList tracks'

albumArtistOrArtist :: Disc -> HTagLib.AlbumArtistOrArtist
albumArtistOrArtist (Disc (track :| _)) = AudioTrack.albumArtistOrArtist track

artist :: Disc -> HTagLib.Artist
artist (Disc (track :| _)) = AudioTrack.atArtist track

albumArtist :: Disc -> HTagLib.AlbumArtist
albumArtist (Disc (track :| _)) = AudioTrack.atAlbumArtist track

disc :: Disc -> Maybe HTagLib.DiscNumber
disc (Disc (track :| _)) = AudioTrack.atDisc track

album :: Disc -> HTagLib.Album
album (Disc (track :| _)) = AudioTrack.atAlbum track

genres :: Disc -> NonEmpty HTagLib.Genre
genres (Disc tracks') = NonEmpty.nubOrd $ AudioTrack.atGenre <$> tracks'

-- | Return the directory if all tracks are in the same one
directory :: Disc -> Maybe (Path.Path Path.Abs Path.Dir)
directory (Disc tracks')
  | length directories == 1 = Just $ head directories
  | otherwise = Nothing
  where
    directories = NonEmpty.nubOrd $ Path.parent . AudioTrack.atFile <$> tracks'

haveSameTag' :: Disc -> Tag.Tag -> Maybe Tag.Tag
haveSameTag' disc' = guarded (not . haveSameTag disc')

haveSameTag :: Disc -> Tag.Tag -> Bool
haveSameTag disc' Tag.Title = haveSameTagWithGetter AudioTrack.atTitle disc'
haveSameTag disc' Tag.Artist = haveSameTagWithGetter AudioTrack.atArtist disc'
haveSameTag disc' Tag.AlbumArtist =
  haveSameTagWithGetter AudioTrack.atAlbumArtist disc'
haveSameTag disc' Tag.Album = haveSameTagWithGetter AudioTrack.atAlbum disc'
haveSameTag disc' Tag.Genre = haveSameTagWithGetter AudioTrack.atGenre disc'
haveSameTag disc' Tag.Year = haveSameTagWithGetter AudioTrack.atYear disc'
haveSameTag disc' Tag.Track = haveSameTagWithGetter AudioTrack.atTrack disc'
haveSameTag disc' Tag.Disc = haveSameTagWithGetter AudioTrack.atDisc disc'

haveSameTagWithGetter ::
  (Eq a) =>
  (AudioTrack.AudioTrack -> a) ->
  Disc ->
  Bool
haveSameTagWithGetter getTagValue (Disc tracks') = all (== firstTag) otherTags
  where
    (firstTag :| otherTags) = getTagValue <$> tracks'
