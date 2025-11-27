{- AUTOCOLLECT.TEST -}
{-# LANGUAGE QuasiQuotes #-}

module Tests.Check.Album
  (
  {- AUTOCOLLECT.TEST.export -}
  )
where

import Check.Album qualified as Album
import Data.List.NonEmpty ((<|))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe qualified as Maybe
import Model.Album qualified as Album
import Model.AudioTrack qualified as AudioTrack
import Model.Tag qualified as Tag
import Path (reldir, relfile, (</>))
import Path qualified
import Sound.HTagLib qualified as HTagLib
import System.IO qualified as System
import Test.Hspec.Expectations (shouldBe)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit qualified as Tasty
import Tests.Common qualified as Common

test :: TestTree
test =
  Tasty.testGroup
    "check cover"
    [ Tasty.testCase "check an album without a cover.jpg" $
        Common.withTenTracksFiles $
          \dir album -> do
            result <- Album.check (Album.HaveCover covers) album
            result `shouldBe` Left (Album.MissingCover dir),
      Tasty.testCase "check an album with a cover.jpg" $
        Common.withTenTracksFiles $
          \dir album -> do
            System.writeFile
              (Path.toFilePath $ dir </> head covers)
              "dummy content"

            result <- Album.check (Album.HaveCover covers) album
            result `shouldBe` Right ()
    ]
  where
    covers = fromList [[relfile|cover.jpg|], [relfile|cover.png|]]

test :: TestTree
test =
  Tasty.testGroup
    "check directory"
    [ Tasty.testCase "an album is in a single directory" $ do
        result <- Album.check Album.InSameDir Common.tenTracksAlbum
        result `shouldBe` Right (),
      Tasty.testCase "an album is in multiple directories" $ do
        let album = Common.tenTracksAlbum
            tracksDir = Maybe.fromJust $ Album.directory album
            otherDir = Path.parent tracksDir </> [reldir|other|]
            (firstHalf, secondHalf) = NonEmpty.splitAt 5 (Album.tracks album)
            secondHalfMoved = moveTo otherDir <$> secondHalf
            album' =
              Maybe.fromJust $
                Album.mkAlbum (fromList $ firstHalf <> secondHalfMoved)
        result <- Album.check Album.InSameDir album'
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
        result <- Album.check (Album.SameTags commonTags) Common.tenTracksAlbum
        result `shouldBe` Right (),
      Tasty.testCase "some tracks have a different tag" $ do
        let album = Common.tenTracksAlbum
        result <- Album.check (Album.SameTags $ Tag.Track <| commonTags) album
        result `shouldBe` Left (Album.SameTagsError $ fromList [Tag.Track])
    ]
  where
    commonTags = fromList [Tag.Genre, Tag.Year, Tag.Artist, Tag.AlbumArtist]

test :: TestTree
test =
  Tasty.testGroup
    "check sequential tracks"
    [ Tasty.testCase "the tracks are sequential" $ do
        result <- Album.check Album.TracksSequential Common.tenTracksAlbum
        result `shouldBe` Right (),
      Tasty.testCase "there are two tracks number 10" $ do
        let (track :| tracks) = Album.tracks Common.tenTracksAlbum
            otherTen = track {AudioTrack.atTrack = HTagLib.mkTrackNumber 10}
            tracks' = otherTen :| tracks
            album' = Maybe.fromJust $ Album.mkAlbum tracks'
        result <- Album.check Album.TracksSequential album'
        result `shouldBe` Left Album.TracksNotSequential
    ]
