{- AUTOCOLLECT.TEST -}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Tests.Check.Album
  (
  {- AUTOCOLLECT.TEST.export -}
  )
where

import AudioTrack qualified
import Check.Album qualified as Album
import Path (reldir, relfile, (</>))
import Path qualified
import Path.IO qualified as Path
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
        withTenTracks $
          \dir tracks -> do
            let cover = [relfile|cover.jpg|]

            result <- Album.check (Album.HaveCover cover) tracks
            result `shouldBe` Left (Album.MissingCover (dir </> cover)),
      Tasty.testCase "check an album with a cover.jpg" $
        withTenTracks $
          \dir tracks -> do
            let cover = [relfile|cover.jpg|]
            System.writeFile (Path.toFilePath $ dir </> cover) "dummy content"

            result <- Album.check (Album.HaveCover cover) tracks
            result `shouldBe` Right ()
    ]

test :: TestTree
test =
  Tasty.testGroup
    "check directory"
    [ Tasty.testCase "check an album with a single directory" $
        withTenTracks $ \_ tracks -> do
          result <- Album.check Album.InSameDir tracks
          result `shouldBe` Right (),
      Tasty.testCase "check an album in multiple directories" $
        Common.withTenTracksFiles $ \dir -> do
          let inputDir = dir </> [reldir|input|]

          -- Take half of the input files
          filenamesBefore <- snd <$> Path.listDir inputDir
          let secondHalf = snd $ splitAt 5 filenamesBefore

          let newDir = dir </> [reldir|other|]
          Path.ensureDir newDir

          -- And move them into 'newDir'
          traverse_ (moveFileIn newDir) secondHalf

          -- Now get all the tracks where they are
          filenames <- snd <$> Path.listDirRecur dir
          trackList <- traverse AudioTrack.getTags filenames

          -- And check if they are in the same dir
          result <- Album.check Album.InSameDir $ fromList trackList
          result `shouldBe` Left Album.NotInSameDir
    ]

moveFileIn ::
  Path.Path Path.Abs Path.Dir -> Path.Path Path.Abs Path.File -> IO ()
moveFileIn dir file = Path.renameFile file $ dir </> Path.filename file

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
