module Album (albumArtistOrArtist, album, disc) where

import AudioTrack qualified
import Data.Text qualified as Text
import Sound.HTagLib qualified as HTagLib
import Sound.HTagLib.Extra qualified as HTagLib

albumArtistOrArtist :: NonEmpty AudioTrack.AudioTrack -> Text
albumArtistOrArtist tracks
  | not $ Text.null albumArtist = albumArtist
  | otherwise = HTagLib.unArtist $ AudioTrack.atArtist firstTrack
  where
    firstTrack = head tracks
    albumArtist = HTagLib.unAlbumArtist $ AudioTrack.atAlbumArtist firstTrack

disc :: NonEmpty AudioTrack.AudioTrack -> Maybe HTagLib.DiscNumber
disc = AudioTrack.atDisc . head

album :: NonEmpty AudioTrack.AudioTrack -> HTagLib.Album
album = AudioTrack.atAlbum . head
