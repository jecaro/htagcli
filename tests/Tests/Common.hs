{-# LANGUAGE QuasiQuotes #-}

module Tests.Common (withTenTracksFiles) where

import Commands qualified
import Path (relfile, (</>))
import Path qualified
import Path.IO qualified as Path
import Sound.HTagLib qualified as HTagLib
import Test.Tasty.HUnit qualified as Tasty

-- | Create a temporary directory and put 10 audio files in the subdirectory
-- 'input'
withTenTracksFiles :: (Path.Path Path.Abs Path.Dir -> IO ()) -> Tasty.Assertion
withTenTracksFiles withTempDir = Path.withSystemTempDir "htagcli" $ \dir -> do
  forM_ [1 .. 10] $ \i -> do
    dstRelFile <- Path.parseRelFile $ "./input/" <> show i <> "-sample.mp3"
    let dstAbsFile = dir </> dstRelFile
    Path.ensureDir $ Path.parent dstAbsFile
    Path.copyFile [relfile|./data/sample.mp3|] dstAbsFile
    Commands.edit
      ( Commands.noEditOptions
          { Commands.eoTrack = Commands.Set <$> HTagLib.mkTrackNumber i
          }
      )
      dstAbsFile
  withTempDir dir
