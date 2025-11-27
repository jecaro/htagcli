{-# LANGUAGE QuasiQuotes #-}

module Tests.Common
  ( tenTracksAlbum,
    tenTracksAlbum',
    withTenTracksFiles,
    withTenTracksFilesInSubdir,
  )
where

import Model.Album qualified as Album
import Model.AudioTrack qualified as AudioTrack
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
  -- | Action to run with the temporary directory and the created album
  (Path.Path Path.Abs Path.Dir -> Album.Album -> IO ()) ->
  Tasty.Assertion
withTenTracksFilesInSubdir subdir withTempDirAndAlbum =
  Path.withSystemTempDir "htagcli" $ \dir -> do
    let album =
          tenTracksAlbum'
            (dir </> subdir)
            (HTagLib.mkAlbum "Album")
            (HTagLib.mkGenre "Pop")
    forM_ (Album.tracks album) $ \track -> do
      let dstAbsFile = AudioTrack.atFile track
      Path.ensureDir $ Path.parent dstAbsFile
      Path.copyFile [relfile|./data/sample.mp3|] dstAbsFile
      AudioTrack.setTags track
    withTempDirAndAlbum dir album

-- | Same with the files directly in the temporary directory
withTenTracksFiles ::
  (Path.Path Path.Abs Path.Dir -> Album.Album -> IO ()) -> Tasty.Assertion
withTenTracksFiles withTempDirAndAlbum =
  withTenTracksFilesInSubdir [reldir|./|] withTempDirAndAlbum

tenTracksAlbum :: Album.Album
tenTracksAlbum =
  tenTracksAlbum'
    [absdir|/path/to|]
    (HTagLib.mkAlbum "Album")
    (HTagLib.mkGenre "Pop")

tenTracksAlbum' ::
  Path.Path Path.Abs Path.Dir -> HTagLib.Album -> HTagLib.Genre -> Album.Album
tenTracksAlbum' dir album genre = Unsafe.fromJust $ do
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
  Album.mkAlbum track
