module Tests.Model.Album (test) where

import Model.Album qualified as Album
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
    "Model.Album"
    [ Tasty.testCase "single disc succeeds" $
        Album.mkAlbum (Common.tenTracksDisc :| []) `shouldSatisfy` isJust,
      Tasty.testCase "two discs with same album and artist succeed" $
        let disc1 = setDiscDisc (HTagLib.mkDiscNumber 1) Common.tenTracksDisc
            disc2 = setDiscDisc (HTagLib.mkDiscNumber 2) disc1
         in Album.mkAlbum (disc1 :| [disc2]) `shouldSatisfy` isJust,
      Tasty.testCase "different album names fail" $
        let disc1 = setDiscDisc (HTagLib.mkDiscNumber 1) Common.tenTracksDisc
            disc2 =
              setDiscDisc (HTagLib.mkDiscNumber 2) $
                setDiscAlbum (HTagLib.mkAlbum "Other Album") disc1
         in Album.mkAlbum (disc1 :| [disc2]) `shouldBe` Nothing,
      Tasty.testCase "different album artists fails" $
        let disc1 = setDiscDisc (HTagLib.mkDiscNumber 1) Common.tenTracksDisc
            disc2 =
              setDiscAlbumArtist
                (HTagLib.mkAlbumArtist "Other Album Artist")
                disc1
         in Album.mkAlbum (disc1 :| [disc2]) `shouldBe` Nothing,
      Tasty.testCase "no album artists but same artist succeeds" $
        let disc1 =
              setDiscAlbumArtist
                (HTagLib.mkAlbumArtist "")
                $ setDiscDisc (HTagLib.mkDiscNumber 1) Common.tenTracksDisc
            disc2 = setDiscDisc (HTagLib.mkDiscNumber 2) disc1
         in Album.mkAlbum (disc1 :| [disc2]) `shouldSatisfy` isJust,
      Tasty.testCase "no album artists but different artist fails" $
        let disc1 =
              setDiscAlbumArtist
                (HTagLib.mkAlbumArtist "")
                $ setDiscDisc (HTagLib.mkDiscNumber 1) Common.tenTracksDisc
            disc2 =
              setDiscDisc (HTagLib.mkDiscNumber 2) $
                setDiscArtist
                  (HTagLib.mkArtist "Other Artist")
                  disc1
         in Album.mkAlbum (disc1 :| [disc2]) `shouldBe` Nothing
    ]
  where
    setDiscAlbum = modifyDisc . setTrackAlbum
    setTrackAlbum album track = track {AudioTrack.atAlbum = album}

    setDiscAlbumArtist = modifyDisc . setTrackAlbumArtist
    setTrackAlbumArtist albumArtist track =
      track {AudioTrack.atAlbumArtist = albumArtist}

    setDiscArtist = modifyDisc . setTrackArtist
    setTrackArtist artist track = track {AudioTrack.atArtist = artist}

    setDiscDisc = modifyDisc . setTrackDisc
    setTrackDisc disc track = track {AudioTrack.atDisc = disc}

    modifyDisc modifyTrack disc =
      Unsafe.fromJust $ Disc.mkDisc $ modifyTrack <$> Disc.tracks disc
