{-# LANGUAGE QuasiQuotes #-}

module Tests.Common
  ( tenTracksDisc,
    tenTracksDisc',
    withTenTracksFiles,
    withTenTracksFilesInSubdir,
    withOneTrackFile,
  )
where

import Model.AudioTrack qualified as AudioTrack
import Model.Disc qualified as Disc
import Path (absdir, reldir, relfile, (</>))
import Path qualified
import Path.IO qualified as Path
import Relude.Unsafe qualified as Unsafe
import Sound.HTagLib qualified as HTagLib
import Sound.HTagLib.Extra qualified as HTagLib
import Test.Tasty.HUnit qualified as Tasty

-- | Create a temporary directory and put 10 audio files in the subdirectory
--   of the system temporary directory
withTenTracksFilesInSubdir ::
  -- | Subdirectory to put the files in
  Path.Path Path.Rel Path.Dir ->
  -- | Action to run with the temporary directory and the created disc
  (Path.Path Path.Abs Path.Dir -> Disc.Disc -> IO ()) ->
  Tasty.Assertion
withTenTracksFilesInSubdir subdir withTempDirAndDisc =
  Path.withSystemTempDir "htagcli" $ \dir -> do
    let d =
          tenTracksDisc'
            (dir </> subdir)
            (HTagLib.mkAlbum "Album")
            (HTagLib.mkGenre "Pop")
    forM_ (Disc.tracks d) $ \track -> do
      let dstAbsFile = AudioTrack.atFile track
      Path.ensureDir $ Path.parent dstAbsFile
      Path.copyFile [relfile|./data/sample.mp3|] dstAbsFile
      AudioTrack.setTags track
    withTempDirAndDisc dir d

-- | Same with the files directly in the temporary directory
withTenTracksFiles ::
  (Path.Path Path.Abs Path.Dir -> Disc.Disc -> IO ()) -> Tasty.Assertion
withTenTracksFiles withTempDirAndDisc =
  withTenTracksFilesInSubdir [reldir|./|] withTempDirAndDisc

withOneTrackFile ::
  (Path.Path Path.Abs Path.Dir -> Path.Path Path.Abs Path.File -> IO ()) ->
  Tasty.Assertion
withOneTrackFile action =
  Path.withSystemTempDir "htagcli" $ \dir -> do
    let file = dir </> [relfile|1-sample.mp3|]
        track =
          AudioTrack.AudioTrack
            { AudioTrack.atFile = file,
              AudioTrack.atTitle = HTagLib.mkTitle "Track 1",
              AudioTrack.atArtist = HTagLib.mkArtist "Artist",
              AudioTrack.atAlbumArtist = HTagLib.mkAlbumArtist "Album Artist",
              AudioTrack.atAlbum = HTagLib.mkAlbum "Album",
              AudioTrack.atGenre = HTagLib.mkGenre "Pop",
              AudioTrack.atYear = HTagLib.mkYear 2025,
              AudioTrack.atTrack = HTagLib.mkTrackNumber 1,
              AudioTrack.atDisc = Nothing
            }
    Path.copyFile [relfile|./data/sample.mp3|] file
    AudioTrack.setTags track
    action dir file

tenTracksDisc :: Disc.Disc
tenTracksDisc =
  tenTracksDisc'
    [absdir|/path/to|]
    (HTagLib.mkAlbum "Album")
    (HTagLib.mkGenre "Pop")

tenTracksDisc' ::
  Path.Path Path.Abs Path.Dir -> HTagLib.Album -> HTagLib.Genre -> Disc.Disc
tenTracksDisc' dir album genre = Unsafe.fromJust $ do
  track <- forM (fromList [1 .. 10]) $ \i -> do
    dstRelFile <- (dir </>) <$> Path.parseRelFile (show i <> "-sample.mp3")
    pure $
      AudioTrack.AudioTrack
        { AudioTrack.atFile = dstRelFile,
          AudioTrack.atTitle = HTagLib.mkTitle $ "Track " <> show i,
          AudioTrack.atArtist = HTagLib.mkArtist "Artist",
          AudioTrack.atAlbumArtist = HTagLib.mkAlbumArtist "Album Artist",
          AudioTrack.atAlbum = album,
          AudioTrack.atGenre = genre,
          AudioTrack.atYear = HTagLib.mkYear 2025,
          AudioTrack.atTrack = HTagLib.mkTrackNumber i,
          AudioTrack.atDisc = Nothing
        }
  Disc.mkDisc track
