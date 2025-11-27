module Model.Album
  ( Album,
    mkAlbum,
    addTrack,
    tracks,
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

import Data.List.NonEmpty ((<|))
import Model.AudioTrack qualified as AudioTrack
import Model.Tag qualified as Tag
import Path qualified
import Sound.HTagLib qualified as HTagLib
import Sound.HTagLib.Extra qualified as HTagLib
import "extra" Data.List.NonEmpty.Extra qualified as NonEmpty

newtype Album = Album (NonEmpty AudioTrack.AudioTrack)
  deriving (Eq, Show)

mkAlbum :: NonEmpty AudioTrack.AudioTrack -> Maybe Album
mkAlbum tracks'@(firstTrack :| otherTracks)
  | allSameAlbum
      && allSameDisc
      && ( ( AudioTrack.haveTag Tag.AlbumArtist firstTrack
               && allSameAlbumArtist
           )
             || allSameArtist
         ) =
      Just $ Album tracks'
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

addTrack :: AudioTrack.AudioTrack -> Album -> Maybe Album
addTrack track (Album tracks') = mkAlbum (track <| tracks')

tracks :: Album -> NonEmpty AudioTrack.AudioTrack
tracks (Album tracks') = tracks'

albumArtistOrArtist :: Album -> Text
albumArtistOrArtist (Album (track :| _)) = AudioTrack.albumArtistOrArtist track

artist :: Album -> HTagLib.Artist
artist (Album (track :| _)) = AudioTrack.atArtist track

albumArtist :: Album -> HTagLib.AlbumArtist
albumArtist (Album (track :| _)) = AudioTrack.atAlbumArtist track

disc :: Album -> Maybe HTagLib.DiscNumber
disc (Album (track :| _)) = AudioTrack.atDisc track

album :: Album -> HTagLib.Album
album (Album (track :| _)) = AudioTrack.atAlbum track

-- | Return the directory if all tracks are in the same one
directory :: Album -> Maybe (Path.Path Path.Abs Path.Dir)
directory (Album tracks')
  | length directories == 1 = Just $ head directories
  | otherwise = Nothing
  where
    directories = NonEmpty.nubOrd $ Path.parent . AudioTrack.atFile <$> tracks'

haveSameTag' :: Album -> Tag.Tag -> Maybe Tag.Tag
haveSameTag' album' = guarded (not . haveSameTag album')

haveSameTag :: Album -> Tag.Tag -> Bool
haveSameTag album' Tag.Title = haveSameTagWithGetter AudioTrack.atTitle album'
haveSameTag album' Tag.Artist = haveSameTagWithGetter AudioTrack.atArtist album'
haveSameTag album' Tag.AlbumArtist =
  haveSameTagWithGetter AudioTrack.atAlbumArtist album'
haveSameTag album' Tag.Album = haveSameTagWithGetter AudioTrack.atAlbum album'
haveSameTag album' Tag.Genre = haveSameTagWithGetter AudioTrack.atGenre album'
haveSameTag album' Tag.Year = haveSameTagWithGetter AudioTrack.atYear album'
haveSameTag album' Tag.Track = haveSameTagWithGetter AudioTrack.atTrack album'
haveSameTag album' Tag.Disc = haveSameTagWithGetter AudioTrack.atDisc album'

haveSameTagWithGetter ::
  (Eq a) =>
  (AudioTrack.AudioTrack -> a) ->
  Album ->
  Bool
haveSameTagWithGetter getTagValue (Album tracks') = all (== firstTag) otherTags
  where
    (firstTag :| otherTags) = getTagValue <$> tracks'
