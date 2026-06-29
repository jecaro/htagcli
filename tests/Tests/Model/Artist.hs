module Tests.Model.Artist (test) where

import Model.Album qualified as Album
import Model.Artist qualified as Artist
import Model.AudioTrack qualified as AudioTrack
import Model.Disc qualified as Disc
import Relude.Unsafe qualified as Unsafe
import Sound.HTagLib qualified as HTagLib
import Sound.HTagLib.Extra qualified as HTagLib
import Test.Hspec.Expectations (shouldBe, shouldSatisfy)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit qualified as Tasty
import Tests.Common qualified as Common

test :: Tasty.TestTree
test =
  Tasty.testGroup
    "Model.Artist"
    [ Tasty.testCase "single album succeeds" $
        Artist.mkArtist (tenTracksAlbum :| []) `shouldSatisfy` isJust,
      Tasty.testCase "two albums same album artist succeeds" $
        let album1 = tenTracksAlbum
            album2 = setAlbumAlbum (HTagLib.mkAlbum "Other Album") album1
         in Artist.mkArtist (album1 :| [album2]) `shouldSatisfy` isJust,
      Tasty.testCase "two Various Artists albums fails" $
        let album1 =
              setAlbumAlbumArtist
                (HTagLib.mkAlbumArtist "Various Artists")
                tenTracksAlbum
            album2 = setAlbumAlbum (HTagLib.mkAlbum "Other Album") album1
         in Artist.mkArtist (album1 :| [album2]) `shouldBe` Nothing,
      Tasty.testCase "Various Artists with different album artist fails" $
        let album1 = tenTracksAlbum
            album2 =
              setAlbumAlbumArtist
                (HTagLib.mkAlbumArtist "Various Artists")
                $ setAlbumAlbum (HTagLib.mkAlbum "Other Album") album1
         in Artist.mkArtist (album1 :| [album2]) `shouldBe` Nothing,
      Tasty.testCase "no album artist but same artist succeeds" $
        let album1 =
              setAlbumAlbumArtist
                (HTagLib.mkAlbumArtist "")
                tenTracksAlbum
            album2 = setAlbumAlbum (HTagLib.mkAlbum "Other Album") album1
         in Artist.mkArtist (album1 :| [album2]) `shouldSatisfy` isJust,
      Tasty.testCase "no album artist but different artist fails" $
        let album1 =
              setAlbumAlbumArtist
                (HTagLib.mkAlbumArtist "")
                tenTracksAlbum
            album2 =
              setAlbumArtist (HTagLib.mkArtist "Other Artist") $
                setAlbumAlbum
                  (HTagLib.mkAlbum "Other Album")
                  album1
         in Artist.mkArtist (album1 :| [album2]) `shouldBe` Nothing
    ]
  where
    tenTracksAlbum = Unsafe.fromJust $ Album.mkAlbum (Common.tenTracksDisc :| [])

    setAlbumAlbumArtist = modifyAlbum . setTrackAlbumArtist

    setAlbumArtist = modifyAlbum . setTrackArtist
    setTrackArtist artist track = track {AudioTrack.atArtist = artist}

    setTrackAlbumArtist albumArtist track =
      track {AudioTrack.atAlbumArtist = albumArtist}
    setAlbumAlbum = modifyAlbum . setTrackAlbum

    setTrackAlbum album track = track {AudioTrack.atAlbum = album}

    modifyAlbum modifyTrack album =
      Unsafe.fromJust $
        Album.mkAlbum $
          modifyDisc modifyTrack <$> Album.discs album
    modifyDisc modifyTrack disc =
      Unsafe.fromJust $ Disc.mkDisc $ modifyTrack <$> Disc.tracks disc
