{- AUTOCOLLECT.TEST -}
{-# LANGUAGE QuasiQuotes #-}

module Tests.Commands
  (
  {- AUTOCOLLECT.TEST.export -}
  )
where

import AudioTrack qualified
import Check.File qualified as File
import Commands qualified
import Path (reldir, relfile, (</>))
import Path qualified
import Path.IO qualified as Path
import Pattern qualified
import System.IO qualified as System
import Tag qualified
import Test.Hspec.Expectations (shouldBe)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit qualified as Tasty
import Tests.Common qualified as Common

test :: TestTree
test =
  Tasty.testGroup
    "fixFilePaths"
    [ Tasty.testCase "dry run" $ Common.withTenTracksFiles $ \dir -> do
        let inputDir = dir </> [reldir|input|]
        filenamesBefore <- snd <$> Path.listDir inputDir

        result <-
          traverse
            (Commands.fixFilePaths' $ fixFilePathsOptions True inputDir)
            filenamesBefore

        -- All files would be renamed
        all isJust result `shouldBe` True

        -- No changes visible on disk
        filenamesAfter <- snd <$> Path.listDir inputDir
        filenamesAfter `shouldBe` filenamesBefore,
      Tasty.testCase "rename and delete empty dirs" $
        Common.withTenTracksFiles $ \dir -> do
          let inputDir = dir </> [reldir|input|]
          filenamesInCurrentDirBefore <- snd <$> Path.listDir inputDir
          listMbPaths <-
            traverse
              (Commands.fixFilePaths' $ fixFilePathsOptions False dir)
              filenamesInCurrentDirBefore

          all isJust listMbPaths `shouldBe` True

          -- All files have been moved, 'input' directory doesn't exist anymore
          exists <- Path.doesDirExist inputDir
          exists `shouldBe` False

          filenamesAfter <- snd <$> Path.listDirRecur dir
          length filenamesAfter `shouldBe` length filenamesInCurrentDirBefore

          checkResults <- traverse check filenamesAfter

          lefts checkResults `shouldBe` mempty
          catMaybes listMbPaths `shouldBe` filenamesAfter,
      Tasty.testCase "rename but keep non-empty dirs" $
        Common.withTenTracksFiles $ \dir -> do
          let inputDir = dir </> [reldir|input|]
              dummy = inputDir </> [relfile|dummy.txt|]
          System.writeFile (Path.toFilePath dummy) "dummy content"

          filenamesInCurrentDirBefore <-
            filter (/= dummy) . snd
              <$> Path.listDir inputDir

          listMbPaths <-
            traverse
              (Commands.fixFilePaths' $ fixFilePathsOptions False dir)
              filenamesInCurrentDirBefore

          all isJust listMbPaths `shouldBe` True

          exists <- Path.doesDirExist inputDir
          exists `shouldBe` True
    ]

check :: (MonadIO m) => Path.Path Path.Abs Path.File -> m (Either File.Error ())
check filename = do
  track <- AudioTrack.getTags filename
  pure $
    File.check
      (File.FilenameMatches pattern Pattern.noFormatting)
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
  Bool -> Path.Path Path.Abs Path.Dir -> Commands.FixFilePathsOptions
fixFilePathsOptions dryRun baseDir =
  Commands.FixFilePathsOptions
    { fiDryRun = dryRun,
      fiBaseDirectory = baseDir </> [reldir|output|],
      fiFormatting = Pattern.noFormatting,
      fiPattern = pattern
    }
