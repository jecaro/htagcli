{- AUTOCOLLECT.TEST -}
{-# LANGUAGE QuasiQuotes #-}

module Tests.Check.Album
  (
  {- AUTOCOLLECT.TEST.export -}
  )
where

import AudioTrack qualified
import Check.Album qualified as Album
import Data.List.NonEmpty ((<|))
import Data.List.NonEmpty qualified as NonEmpty
import Path (reldir, relfile, (</>))
import Path qualified
import Path.IO qualified as Path
import Sound.HTagLib qualified as HTagLib
import System.IO qualified as System
import Tag qualified
import Test.Hspec.Expectations (shouldBe)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit qualified as Tasty
import Tests.Common qualified as Common

test :: TestTree
test =
  Tasty.testGroup
    "check cover"
    [ Tasty.testCase "check an album without a cover.jpg" $
        withTenTracks $
          \dir tracks -> do
            result <- Album.check (Album.HaveCover covers) tracks
            result `shouldBe` Left (Album.MissingCover dir),
      Tasty.testCase "check an album with a cover.jpg" $
        withTenTracks $
          \dir tracks -> do
            System.writeFile
              (Path.toFilePath $ dir </> NonEmpty.head covers)
              "dummy content"

            result <- Album.check (Album.HaveCover covers) tracks
            result `shouldBe` Right ()
    ]
  where
    covers = fromList [[relfile|cover.jpg|], [relfile|cover.png|]]

test :: TestTree
test =
  Tasty.testGroup
    "check directory"
    [ Tasty.testCase "an album is in a single directory" $ do
        tracks <- Common.tenTracks
        result <- Album.check Album.InSameDir tracks
        result `shouldBe` Right (),
      Tasty.testCase "an album is in multiple directories" $ do
        tracks <- Common.tenTracks
        let tracksDir = Path.parent . AudioTrack.atFile $ NonEmpty.head tracks
            otherDir = Path.parent tracksDir </> [reldir|other|]
            (firstHalf, secondHalf) = NonEmpty.splitAt 5 tracks
            secondHalfMoved = moveTo otherDir <$> secondHalf
        result <-
          Album.check
            Album.InSameDir
            $ fromList
            $ firstHalf <> secondHalfMoved
        result `shouldBe` Left Album.NotInSameDir
    ]
  where
    moveTo newDir track =
      track
        { AudioTrack.atFile = newDir </> Path.filename (AudioTrack.atFile track)
        }

test :: TestTree
test =
  Tasty.testGroup
    "check same tags"
    [ Tasty.testCase "all tracks have the same tags" $ do
        tracks <- Common.tenTracks
        result <- Album.check (Album.SameTags commonTags) tracks
        result `shouldBe` Right (),
      Tasty.testCase "some tracks have a different tag" $ do
        tracks <- Common.tenTracks
        result <- Album.check (Album.SameTags $ Tag.Track <| commonTags) tracks
        result `shouldBe` Left (Album.SameTagsError $ fromList [Tag.Track])
    ]
  where
    commonTags = fromList [Tag.Genre, Tag.Year, Tag.Artist, Tag.AlbumArtist]

test :: TestTree
test =
  Tasty.testGroup
    "check sequential tracks"
    [ Tasty.testCase "the tracks are sequential" $ do
        tracks <- Common.tenTracks
        result <- Album.check Album.TracksSequential tracks
        result `shouldBe` Right (),
      Tasty.testCase "there are two tracks number 10" $ do
        (track :| tracks) <- Common.tenTracks
        let otherTen = track {AudioTrack.atTrack = HTagLib.mkTrackNumber 10}
            tracks' = otherTen :| tracks
        result <- Album.check Album.TracksSequential tracks'
        result `shouldBe` Left Album.TracksNotSequential
    ]

withTenTracks ::
  ( Path.Path Path.Abs Path.Dir ->
    NonEmpty AudioTrack.AudioTrack ->
    IO ()
  ) ->
  Tasty.Assertion
withTenTracks a = Common.withTenTracksFiles $ \dir -> do
  let inputDir = dir </> [reldir|input|]
  filenames <- snd <$> Path.listDir inputDir
  trackList <- traverse AudioTrack.getTags filenames
  a inputDir $ fromList trackList
