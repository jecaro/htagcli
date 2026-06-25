{-# LANGUAGE QuasiQuotes #-}

module Tests.Check.Album (test) where

import Check.Album qualified as Album
import Model.Album qualified as Album
import Model.AudioTrack qualified as AudioTrack
import Model.Disc qualified as Disc
import Model.Tag qualified as Tag
import Path (absdir, (</>))
import Path qualified
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
    "Check.Album"
    [ testCheckDiscsSequential,
      testCheckSameTags
    ]

testCheckDiscsSequential :: Tasty.TestTree
testCheckDiscsSequential =
  Tasty.testGroup
    "check discs sequential"
    [ Tasty.testCase "single disc without a disc number" $ do
        let album = singleDiscAlbum Common.tenTracksDisc
        Album.check Album.DiscsSequential album
          `shouldBe` Right (),
      Tasty.testCase "two discs with sequential numbers" $ do
        let disc1 = discWithNumber albumTag genre (HTagLib.mkDiscNumber 1) dir1
            disc2 = discWithNumber albumTag genre (HTagLib.mkDiscNumber 2) dir2
            album = Unsafe.fromJust $ Album.mkAlbum (disc1 :| [disc2])
        Album.check Album.DiscsSequential album
          `shouldBe` Right (),
      Tasty.testCase "two discs without disc numbers" $ do
        let disc1 = discWithNumber albumTag genre Nothing dir1
            disc2 = discWithNumber albumTag genre Nothing dir2
            album = Unsafe.fromJust $ Album.mkAlbum (disc1 :| [disc2])
        Album.check Album.DiscsSequential album
          `shouldBe` Left Album.DiscsNotSequential,
      Tasty.testCase "two discs with non-sequential numbers" $ do
        let disc1 = discWithNumber albumTag genre (HTagLib.mkDiscNumber 1) dir1
            disc3 = discWithNumber albumTag genre (HTagLib.mkDiscNumber 3) dir2
            album = Unsafe.fromJust $ Album.mkAlbum (disc1 :| [disc3])
        Album.check Album.DiscsSequential album
          `shouldBe` Left Album.DiscsNotSequential
    ]
  where
    albumTag = HTagLib.mkAlbum "Album"
    genre = HTagLib.mkGenre "Pop"
    dir1 = [absdir|/path/to/disc1|]
    dir2 = [absdir|/path/to/disc2|]

testCheckSameTags :: Tasty.TestTree
testCheckSameTags =
  Tasty.testGroup
    "check same tags"
    [ Tasty.testCase "all tracks in all discs have the same genre" $ do
        let album = singleDiscAlbum Common.tenTracksDisc
        Album.check (Album.SameTags $ fromList [Tag.Genre]) album
          `shouldBe` Right (),
      Tasty.testCase "a disc has tracks with different track numbers" $ do
        let album = singleDiscAlbum Common.tenTracksDisc
        Album.check (Album.SameTags $ fromList [Tag.Track]) album
          `shouldBe` Left (Album.SameTagsError $ fromList [Tag.Track])
    ]

singleDiscAlbum :: Disc.Disc -> Album.Album
singleDiscAlbum d = Unsafe.fromJust $ Album.mkAlbum (d :| [])

discWithNumber ::
  HTagLib.Album ->
  HTagLib.Genre ->
  Maybe HTagLib.DiscNumber ->
  Path.Path Path.Abs Path.Dir ->
  Disc.Disc
discWithNumber albumTag genre discNum dir = Unsafe.fromJust $ do
  track <- forM (fromList [1 .. 10]) $ \i -> do
    dstRelFile <- (dir </>) <$> Path.parseRelFile (show i <> "-sample.mp3")
    pure $
      AudioTrack.AudioTrack
        { AudioTrack.atFile = dstRelFile,
          AudioTrack.atTitle = HTagLib.mkTitle $ "Track " <> show i,
          AudioTrack.atArtist = HTagLib.mkArtist "Artist",
          AudioTrack.atAlbumArtist = HTagLib.mkAlbumArtist "Album Artist",
          AudioTrack.atAlbum = albumTag,
          AudioTrack.atGenre = genre,
          AudioTrack.atYear = HTagLib.mkYear 2025,
          AudioTrack.atTrack = HTagLib.mkTrackNumber i,
          AudioTrack.atDisc = discNum
        }
  Disc.mkDisc track
