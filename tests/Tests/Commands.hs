{-# LANGUAGE QuasiQuotes #-}

module Tests.Commands (test) where

import Check.Track qualified as Track
import Commands qualified
import Data.List qualified as List
import Model.AudioTrack qualified as AudioTrack
import Model.Pattern qualified as Pattern
import Model.Tag qualified as Tag
import Path (reldir, relfile, (</>))
import Path qualified
import Path.IO qualified as Path
import Relude.Unsafe qualified as Unsafe
import System.IO qualified as System
import Test.Hspec.Expectations (shouldBe)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit qualified as Tasty
import Tests.Common qualified as Common
import UnliftIO.Exception qualified as Exception

test :: Tasty.TestTree
test = Tasty.testGroup "Commands" [testFixFilePaths]

testFixFilePaths :: Tasty.TestTree
testFixFilePaths =
  Tasty.testGroup
    "fixFilePaths"
    [ Tasty.testCase "dry run" $
        Common.withTenTracksFilesInSubdir [reldir|./|] $ \dir _ -> do
          let inputDir = dir
          filenamesBefore <- snd <$> Path.listDir inputDir

          result <-
            traverse
              (Commands.fixFilePaths' $ fixFilePathsOptions True False inputDir)
              filenamesBefore

          -- All files would be renamed
          all isJust result `shouldBe` True

          -- No changes visible on disk
          filenamesAfter <- snd <$> Path.listDir inputDir
          filenamesAfter `shouldBe` filenamesBefore,
      Tasty.testCase "rename and delete empty dirs" $
        Common.withTenTracksFilesInSubdir [reldir|./input|] $ \dir _ -> do
          let inputDir = dir </> [reldir|input|]
          filenamesInCurrentDirBefore <- snd <$> Path.listDir inputDir
          listMbPaths <-
            traverse
              (Commands.fixFilePaths' $ fixFilePathsOptions False False dir)
              filenamesInCurrentDirBefore

          all isJust listMbPaths `shouldBe` True

          -- All files have been moved, 'input' directory doesn't exist anymore
          exists <- Path.doesDirExist inputDir
          exists `shouldBe` False

          filenamesAfter <- snd <$> Path.listDirRecur dir
          length filenamesAfter `shouldBe` length filenamesInCurrentDirBefore

          checkResults <- traverse check filenamesAfter

          lefts checkResults `shouldBe` mempty
          List.sort (catMaybes listMbPaths) `shouldBe` List.sort filenamesAfter,
      Tasty.testCase "rename but keep non-empty dirs" $
        Common.withTenTracksFilesInSubdir [reldir|./input|] $ \dir _ -> do
          let inputDir = dir </> [reldir|input|]
              dummy = inputDir </> [relfile|dummy.txt|]
          System.writeFile (Path.toFilePath dummy) "dummy content"

          filenamesInCurrentDirBefore <-
            filter (/= dummy) . snd
              <$> Path.listDir inputDir

          listMbPaths <-
            traverse
              (Commands.fixFilePaths' $ fixFilePathsOptions False False dir)
              filenamesInCurrentDirBefore

          all isJust listMbPaths `shouldBe` True

          exists <- Path.doesDirExist inputDir
          exists `shouldBe` True,
      Tasty.testCase "rename but dont move the cover image" $ do
        Common.withTenTracksFilesInSubdir [reldir|./input|] $ \dir _ -> do
          let inputDir = dir </> [reldir|input|]
              cover = inputDir </> [relfile|cover.jpg|]
          Path.copyFile [relfile|./data/cover.png|] cover

          filenamesInCurrentDirBefore <-
            filter (/= cover) . snd
              <$> Path.listDir inputDir

          traverse_
            (Commands.fixFilePaths' $ fixFilePathsOptions False False dir)
            filenamesInCurrentDirBefore

          exists <- Path.doesFileExist cover
          exists `shouldBe` True,
      Tasty.testCase "fails when target already exists" $
        testTargetAlreadyExists False,
      Tasty.testCase "fails when target already exists (dry run)" $
        testTargetAlreadyExists True,
      Tasty.testCase "rename and move the cover image" $
        Common.withTenTracksFilesInSubdir [reldir|./input|] $ \dir _ -> do
          let inputDir = dir </> [reldir|input|]
              relCover = [relfile|cover.png|]
              cover = inputDir </> relCover
          Path.copyFile [relfile|./data/cover.png|] cover

          filenamesInCurrentDirBefore <-
            filter (/= cover) . snd
              <$> Path.listDir inputDir

          listMbPaths <-
            traverse
              (Commands.fixFilePaths' $ fixFilePathsOptions False True dir)
              filenamesInCurrentDirBefore

          oldCoverExists <- Path.doesFileExist cover
          oldCoverExists `shouldBe` False

          let firstRenamedFile = Unsafe.fromJust $ asum listMbPaths
              firstDir = Path.parent firstRenamedFile

          newCoverExists <- Path.doesFileExist (firstDir </> relCover)
          newCoverExists `shouldBe` True
    ]

testTargetAlreadyExists :: Bool -> Tasty.Assertion
testTargetAlreadyExists dryRun =
  Common.withOneTrackFile $ \dir file -> do
    targetFile <-
      Unsafe.fromJust
        <$> Commands.fixFilePaths' (fixFilePathsOptions True False dir) file
    Path.ensureDir $ Path.parent targetFile
    -- Create a dummy file where the file should be moved
    System.writeFile (Path.toFilePath targetFile) ""
    result <-
      Exception.try $
        Commands.fixFilePaths' (fixFilePathsOptions dryRun False dir) file
    result `shouldBe` Left (Commands.TargetFileAlreadyExists targetFile)

check :: (MonadIO m) => Path.Path Path.Abs Path.File -> m (Either Track.Error ())
check filename = do
  track <- AudioTrack.getTags filename
  pure $
    Track.check
      (Track.FilenameMatches pattern Pattern.noFormatting)
      track

pattern :: Pattern.Pattern
pattern =
  fromList
    [ fromList [Pattern.FrPlaceholder (Pattern.PlTag Tag.Artist)],
      fromList [Pattern.FrPlaceholder (Pattern.PlTag Tag.Album)],
      fromList
        [ Pattern.FrPlaceholder (Pattern.PlTag Tag.Track),
          Pattern.FrText "-sample"
        ]
    ]

fixFilePathsOptions ::
  Bool -> Bool -> Path.Path Path.Abs Path.Dir -> Commands.FixFilePathsOptions
fixFilePathsOptions dryRun moveCover baseDir =
  Commands.FixFilePathsOptions
    { fiDryRun = dryRun,
      fiBaseDirectory = baseDir </> [reldir|output|],
      fiFormatting = Pattern.noFormatting,
      fiPattern = pattern,
      fiCoverImages = guard moveCover *> Just (fromList [[relfile|cover.png|]])
    }
