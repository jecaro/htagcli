module Tests.Model.Disc (test) where

import Model.AudioTrack qualified as AudioTrack
import Model.Disc qualified as Disc
import Sound.HTagLib qualified as HTagLib
import Sound.HTagLib.Extra qualified as HTagLib
import Test.Hspec.Expectations (shouldBe, shouldSatisfy)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit qualified as Tasty
import Tests.Common qualified as Common

test :: Tasty.TestTree
test =
  Tasty.testGroup
    "Model.Disc"
    [ Tasty.testCase "valid tracks succeeds" $
        Disc.mkDisc (Disc.tracks Common.tenTracksDisc) `shouldSatisfy` isJust,
      Tasty.testCase "different album names fails" $
        let tracks =
              setFirstTrackAlbum (HTagLib.mkAlbum "Other Album") $
                Disc.tracks Common.tenTracksDisc
         in Disc.mkDisc tracks `shouldBe` Nothing,
      Tasty.testCase "different disc numbers fails" $
        let tracks =
              setFirstTrackDisc (HTagLib.mkDiscNumber 2) $
                setTrackDisc (HTagLib.mkDiscNumber 1)
                  <$> Disc.tracks Common.tenTracksDisc
         in Disc.mkDisc tracks `shouldBe` Nothing,
      Tasty.testCase "different album artist fails" $
        let tracks =
              setFirstTrackAlbumArtist
                (HTagLib.mkAlbumArtist "Other Album Artist")
                $ Disc.tracks Common.tenTracksDisc
         in Disc.mkDisc tracks `shouldBe` Nothing,
      Tasty.testCase "no album artist but same artist succeeds" $
        let tracks =
              setTrackAlbumArtist (HTagLib.mkAlbumArtist "")
                <$> Disc.tracks Common.tenTracksDisc
         in Disc.mkDisc tracks `shouldSatisfy` isJust,
      Tasty.testCase "no album artist but different artist fails" $
        let tracks =
              setFirstTrackArtist (HTagLib.mkArtist "Other Artist") $
                setTrackAlbumArtist (HTagLib.mkAlbumArtist "")
                  <$> Disc.tracks Common.tenTracksDisc
         in Disc.mkDisc tracks `shouldBe` Nothing
    ]
  where
    setFirstTrackDisc = modifyFirstTrack . setTrackDisc
    setTrackDisc disc track = track {AudioTrack.atDisc = disc}

    setFirstTrackAlbum = modifyFirstTrack . setTrackAlbum
    setTrackAlbum album track = track {AudioTrack.atAlbum = album}

    setFirstTrackAlbumArtist = modifyFirstTrack . setTrackAlbumArtist
    setTrackAlbumArtist albumArtist track =
      track {AudioTrack.atAlbumArtist = albumArtist}

    setFirstTrackArtist = modifyFirstTrack . setTrackArtist
    setTrackArtist artist track = track {AudioTrack.atArtist = artist}

    modifyFirstTrack changeTrack (track :| tracks) = changeTrack track :| tracks
