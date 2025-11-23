{- AUTOCOLLECT.TEST -}
{-# LANGUAGE QuasiQuotes #-}

module Tests.Check.Artist
  (
  {- AUTOCOLLECT.TEST.export -}
  )
where

import AudioTrack qualified
import Check.Artist qualified as Artist
import Data.List.NonEmpty qualified as NonEmpty
import Path (absfile)
import Sound.HTagLib qualified as HTagLib
import Sound.HTagLib.Extra qualified as HTagLib
import Test.Hspec.Expectations (shouldBe)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit qualified as Tasty

test :: TestTree
test =
  Tasty.testGroup
    "check artist"
    [ Tasty.testCase "two albums of the same artist with the same genre" $ do
        let album1 =
              mkTenTracks (HTagLib.mkAlbum "album-1") (HTagLib.mkGenre "Rock")
            album2 =
              mkTenTracks (HTagLib.mkAlbum "album-2") (HTagLib.mkGenre "Rock")
        Artist.check Artist.SameGenre (fromList [album1, album2])
          `shouldBe` Right (),
      Tasty.testCase "two albums of the same artist with different genres" $ do
        let album1 =
              mkTenTracks (HTagLib.mkAlbum "album-1") (HTagLib.mkGenre "Rock")
            album2 =
              mkTenTracks (HTagLib.mkAlbum "album-2") (HTagLib.mkGenre "Pop")
        Artist.check Artist.SameGenre (fromList [album1, album2])
          `shouldBe` Left Artist.SameGenreError
    ]

mkTenTracks ::
  HTagLib.Album -> HTagLib.Genre -> NonEmpty.NonEmpty AudioTrack.AudioTrack
mkTenTracks album genre = NonEmpty.fromList $ mkTrack album genre <$> [1 .. 10]

mkTrack :: HTagLib.Album -> HTagLib.Genre -> Int -> AudioTrack.AudioTrack
mkTrack album genre n =
  AudioTrack.AudioTrack
    { AudioTrack.atFile = [absfile|/some/path/sample.mp3|],
      AudioTrack.atArtist = HTagLib.mkArtist "Artist",
      AudioTrack.atAlbumArtist = HTagLib.mkAlbumArtist "Album Artist",
      AudioTrack.atAlbum = album,
      AudioTrack.atTrack = HTagLib.mkTrackNumber n,
      AudioTrack.atDisc = Nothing,
      AudioTrack.atTitle = HTagLib.mkTitle $ "Title " <> show n,
      AudioTrack.atYear = Nothing,
      AudioTrack.atGenre = genre
    }
