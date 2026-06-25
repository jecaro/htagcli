{-# LANGUAGE QuasiQuotes #-}

module Tests.Check.Disc (test) where

import Check.Disc qualified as Disc
import Data.List.NonEmpty ((<|))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe qualified as Maybe
import Model.AudioTrack qualified as AudioTrack
import Model.Disc qualified as Disc
import Model.Tag qualified as Tag
import Path (reldir, relfile, (</>))
import Path qualified
import Path.IO qualified as Path
import Sound.HTagLib qualified as HTagLib
import Test.Hspec.Expectations (shouldBe, shouldSatisfy)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit qualified as Tasty
import Tests.Common qualified as Common

test :: Tasty.TestTree
test =
  Tasty.testGroup
    "Check.Disc"
    [ testCheckCover,
      testCheckDirectory,
      testCheckSameTags,
      testCheckSequential
    ]

testCheckCover :: Tasty.TestTree
testCheckCover =
  Tasty.testGroup
    "check cover"
    [ Tasty.testCase "check a disc without a cover.png" $
        Common.withTenTracksFiles $
          \dir disc -> do
            result <- Disc.check (Disc.HaveCover coverNoSize) disc
            result `shouldBe` Left (Disc.MissingCover dir),
      Tasty.testCase "check a disc with a cover.png" $
        Common.withTenTracksFiles $
          \dir disc -> do
            let coverFile = [relfile|./data/cover.png|]
            Path.copyFile coverFile $ dir </> Path.filename coverFile

            result <- Disc.check (Disc.HaveCover coverNoSize) disc
            result `shouldBe` Right (),
      Tasty.testCase "check a disc with a cover.png but too small" $
        Common.withTenTracksFiles $
          \dir disc -> do
            let coverFile = [relfile|./data/cover.png|]
            Path.copyFile coverFile $ dir </> Path.filename coverFile

            result <- Disc.check (Disc.HaveCover coverTooSmall) disc
            result `shouldSatisfy` isBadCoverSize,
      Tasty.testCase "check a disc with a cover.png but too big" $
        Common.withTenTracksFiles $
          \dir disc -> do
            let coverFile = [relfile|./data/cover.png|]
            Path.copyFile coverFile $ dir </> Path.filename coverFile

            result <- Disc.check (Disc.HaveCover coverTooBig) disc
            result `shouldSatisfy` isBadCoverSize
    ]
  where
    isBadCoverSize (Left (Disc.BadCoverSize _ _)) = True
    isBadCoverSize _ = False
    coverNoSize =
      Disc.Cover
        { Disc.coPaths = covers,
          Disc.coMinSize = Nothing,
          Disc.coMaxSize = Nothing
        }
    coverTooSmall =
      Disc.Cover
        { Disc.coPaths = covers,
          Disc.coMinSize = Just (Disc.Size 200 200),
          Disc.coMaxSize = Nothing
        }
    coverTooBig =
      Disc.Cover
        { Disc.coPaths = covers,
          Disc.coMinSize = Nothing,
          Disc.coMaxSize = Just (Disc.Size 50 50)
        }
    covers = fromList [[relfile|cover.jpg|], [relfile|cover.png|]]

testCheckDirectory :: Tasty.TestTree
testCheckDirectory =
  Tasty.testGroup
    "check directory"
    [ Tasty.testCase "a disc is in a single directory" $ do
        result <- Disc.check Disc.InSameDir Common.tenTracksDisc
        result `shouldBe` Right (),
      Tasty.testCase "a disc is in multiple directories" $ do
        let d = Common.tenTracksDisc
            tracksDir = Maybe.fromJust $ Disc.directory d
            otherDir = Path.parent tracksDir </> [reldir|other|]
            (firstHalf, secondHalf) = NonEmpty.splitAt 5 (Disc.tracks d)
            secondHalfMoved = moveTo otherDir <$> secondHalf
            d' =
              Maybe.fromJust $
                Disc.mkDisc (fromList $ firstHalf <> secondHalfMoved)
        result <- Disc.check Disc.InSameDir d'
        result `shouldBe` Left Disc.NotInSameDir
    ]
  where
    moveTo newDir track =
      track
        { AudioTrack.atFile = newDir </> Path.filename (AudioTrack.atFile track)
        }

testCheckSameTags :: Tasty.TestTree
testCheckSameTags =
  Tasty.testGroup
    "check same tags"
    [ Tasty.testCase "all tracks have the same tags" $ do
        result <- Disc.check (Disc.SameTags commonTags) Common.tenTracksDisc
        result `shouldBe` Right (),
      Tasty.testCase "some tracks have a different tag" $ do
        let d = Common.tenTracksDisc
        result <- Disc.check (Disc.SameTags $ Tag.Track <| commonTags) d
        result `shouldBe` Left (Disc.SameTagsError $ fromList [Tag.Track])
    ]
  where
    commonTags = fromList [Tag.Genre, Tag.Year, Tag.Artist, Tag.AlbumArtist]

testCheckSequential :: Tasty.TestTree
testCheckSequential =
  Tasty.testGroup
    "check sequential tracks"
    [ Tasty.testCase "the tracks are sequential" $ do
        result <- Disc.check Disc.TracksSequential Common.tenTracksDisc
        result `shouldBe` Right (),
      Tasty.testCase "there are two tracks number 10" $ do
        let (track :| tracks) = Disc.tracks Common.tenTracksDisc
            otherTen = track {AudioTrack.atTrack = HTagLib.mkTrackNumber 10}
            tracks' = otherTen :| tracks
            d' = Maybe.fromJust $ Disc.mkDisc tracks'
        result <- Disc.check Disc.TracksSequential d'
        result `shouldBe` Left Disc.TracksNotSequential
    ]
