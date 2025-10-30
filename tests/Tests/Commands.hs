{- AUTOCOLLECT.TEST -}
{-# LANGUAGE QuasiQuotes #-}

module Tests.Commands
  (
  {- AUTOCOLLECT.TEST.export -}
  )
where

import AudioTrack qualified
import Check qualified
import Commands qualified
import Path (relfile, (</>))
import Path qualified
import Path.IO qualified as Path
import Pattern qualified
import Sound.HTagLib qualified as HTagLib
import Tag qualified
import Test.Hspec.Expectations (shouldBe)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit qualified as Tasty

test :: TestTree
test =
  Tasty.testGroup
    "fixFilePaths"
    [ Tasty.testCase "dry run" $ withTenTracksFiles $ \dir -> do
        filenamesBefore <- snd <$> Path.listDir dir

        result <-
          traverse
            (Commands.fixFilePaths' $ fixFilePathsOptions True dir)
            filenamesBefore

        -- All files would be renamed
        all isJust result `shouldBe` True

        -- No changes visible on disk
        filenamesAfter <- snd <$> Path.listDir dir
        filenamesAfter `shouldBe` filenamesBefore,
      Tasty.testCase "rename" $ withTenTracksFiles $ \dir -> do
        filenamesInCurrentDirBefore <- snd <$> Path.listDir dir
        listMbPaths <-
          traverse
            (Commands.fixFilePaths' $ fixFilePathsOptions False dir)
            filenamesInCurrentDirBefore

        all isJust listMbPaths `shouldBe` True

        -- All files have been moved
        filenamesInCurrentDirAfter <- snd <$> Path.listDir dir
        filenamesInCurrentDirAfter `shouldBe` mempty

        filenamesAfter <- snd <$> Path.listDirRecur dir
        length filenamesAfter `shouldBe` length filenamesInCurrentDirBefore

        checkResults <- traverse check filenamesAfter

        lefts checkResults `shouldBe` mempty
        catMaybes listMbPaths `shouldBe` filenamesAfter
    ]

check :: (MonadIO m) => Path.Path Path.Abs Path.File -> m (Either Check.Error ())
check filename = do
  track <- AudioTrack.getTags filename
  pure $
    Check.check
      (Check.FilenameMatches pattern Pattern.noFormatting)
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
      fiBaseDirectory = baseDir,
      fiFormatting = Pattern.noFormatting,
      fiPattern = pattern
    }

withTenTracksFiles :: (Path.Path Path.Abs Path.Dir -> IO ()) -> Tasty.Assertion
withTenTracksFiles withTempDir = Path.withSystemTempDir "htagcli" $ \dir -> do
  forM_ [1 .. 10] $ \i -> do
    dstRelFile <- Path.parseRelFile $ show i <> "-sample.mp3"
    let dstAbsFile = dir </> dstRelFile
    Path.copyFile [relfile|./data/sample.mp3|] dstAbsFile
    Commands.edit
      ( Commands.noEditOptions
          { Commands.eoTrack = Commands.Set <$> HTagLib.mkTrackNumber i
          }
      )
      dstAbsFile
  withTempDir dir
