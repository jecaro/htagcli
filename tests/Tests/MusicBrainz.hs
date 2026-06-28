module Tests.MusicBrainz (test) where

import Data.Either.Extra qualified as Either
import Data.List.NonEmpty qualified as NonEmpty
import Model.Album qualified as Album
import Model.AudioTrack qualified as AudioTrack
import MusicBrainz qualified
import MusicBrainz.Types qualified as MusicBrainz
import Relude.Unsafe qualified as Unsafe
import Sound.HTagLib qualified as HTagLib
import Sound.HTagLib.Extra qualified as HTagLib
import Test.Hspec.Expectations (shouldBe)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit qualified as Tasty
import Tests.Common qualified as Common

test :: Tasty.TestTree
test =
  Tasty.testGroup
    "MusicBrainz"
    [ Tasty.testCase "happy path" $ do
        let initialAlbum = Unsafe.fromJust $ Album.mkAlbum (Common.tenTracksDisc :| [])
            detail = mkDetail 1 10
            taggedTracks = Either.fromRight' $ MusicBrainz.tagAlbum detail initialAlbum
        forM_ (zip (toList taggedTracks) [1 ..]) $ \(track, n :: Int) -> do
          let artist = HTagLib.unArtist (AudioTrack.atArtist track)
          artist `shouldBe` "Test Track Artist"

          let albumArtist = HTagLib.unAlbumArtist (AudioTrack.atAlbumArtist track)
          albumArtist `shouldBe` "Test Album Artist"

          let album = HTagLib.unAlbum (AudioTrack.atAlbum track)
          album `shouldBe` "Test Album"

          let title = HTagLib.unTitle (AudioTrack.atTitle track)
          title `shouldBe` "Test Track " <> show n,
      Tasty.testCase "mismatched media count" $ do
        let album = Unsafe.fromJust $ Album.mkAlbum (Common.tenTracksDisc :| [])
            detail = mkDetail 2 10
        MusicBrainz.tagAlbum detail album
          `shouldBe` Left MusicBrainz.MismatchedMediaCount,
      Tasty.testCase "mismatched track count" $ do
        let album = Unsafe.fromJust $ Album.mkAlbum (Common.tenTracksDisc :| [])
            detail = mkDetail 1 9
        MusicBrainz.tagAlbum detail album
          `shouldBe` Left (MusicBrainz.MismatchedTrackCount 1)
    ]

mkDetail :: Int -> Int -> MusicBrainz.ReleaseDetail
mkDetail nMedia nTracksPerMedia =
  MusicBrainz.ReleaseDetail
    { MusicBrainz.rdId = "test-id",
      MusicBrainz.rdTitle = "Test Album",
      MusicBrainz.rdArtistCredit = mkArtistCredit "Test Album Artist",
      MusicBrainz.rdDate = Nothing,
      MusicBrainz.rdMedia =
        NonEmpty.fromList $ mkMedia nTracksPerMedia <$> [1 .. nMedia]
    }

mkMedia :: Int -> Int -> MusicBrainz.Media
mkMedia nTracks pos =
  MusicBrainz.Media
    { MusicBrainz.mePosition = pos,
      MusicBrainz.meTracks =
        NonEmpty.fromList $ mkTrack <$> [1 .. nTracks]
    }

mkArtistCredit :: Text -> NonEmpty MusicBrainz.ArtistCredit
mkArtistCredit name =
  MusicBrainz.ArtistCredit
    { MusicBrainz.acName = name,
      MusicBrainz.acJoinphrase = Nothing
    }
    :| []

mkTrack :: Int -> MusicBrainz.Track
mkTrack n =
  MusicBrainz.Track
    { MusicBrainz.trPosition = n,
      MusicBrainz.trRecording =
        MusicBrainz.Recording
          { MusicBrainz.rcTitle = "Test Track " <> show n,
            MusicBrainz.rcArtistCredit = mkArtistCredit "Test Track Artist"
          }
    }
