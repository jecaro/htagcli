{-# LANGUAGE QuasiQuotes #-}

module Tests.Common (withTenTracksFiles, tenTracks) where

import AudioTrack qualified
import Commands qualified
import Path (relfile, (</>))
import Path qualified
import Path.IO qualified as Path
import SetTagsOptions qualified
import Sound.HTagLib qualified as HTagLib
import Sound.HTagLib.Extra qualified as HTagLib
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
    Commands.setTags
      ( SetTagsOptions.noSetTagsOptions
          { SetTagsOptions.seTrack =
              SetTagsOptions.Set <$> HTagLib.mkTrackNumber i
          }
      )
      dstAbsFile
  withTempDir dir

tenTracks :: IO (NonEmpty AudioTrack.AudioTrack)
tenTracks =
  forM (fromList [1 .. 10]) $ \i -> do
    dstRelFile <- Path.parseAbsFile $ "/path/to/" <> show i <> "-sample.mp3"
    pure $
      AudioTrack.AudioTrack
        { AudioTrack.atFile = dstRelFile,
          AudioTrack.atTitle = HTagLib.mkTitle $ "Track " <> show i,
          AudioTrack.atArtist = HTagLib.mkArtist "Artist",
          AudioTrack.atAlbumArtist = HTagLib.mkAlbumArtist "",
          AudioTrack.atAlbum = HTagLib.mkAlbum "Album",
          AudioTrack.atGenre = HTagLib.mkGenre "Pop",
          AudioTrack.atYear = HTagLib.mkYear 2025,
          AudioTrack.atTrack = HTagLib.mkTrackNumber i,
          AudioTrack.atDisc = Nothing
        }
