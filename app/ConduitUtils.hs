module ConduitUtils (runConduitWithProgress, albumC, artistC) where

import Conduit ((.|))
import Conduit qualified
import Data.Text qualified as Text
import Model.Album qualified as Album
import Model.Artist qualified as Artist
import Model.AudioTrack qualified as AudioTrack
import Options qualified
import Path qualified
import Path.IO qualified as Path
import Progress qualified

runConduitWithProgress ::
  Options.FilesOrDirectory ->
  Conduit.ConduitT
    (Path.Path Path.Abs Path.File)
    Void
    (Conduit.ResourceT IO)
    a ->
  IO a
runConduitWithProgress = Progress.connectWithProgress . fileOrDirectoryC

fileOrDirectoryC ::
  (Conduit.MonadResource m, Conduit.MonadThrow m) =>
  Options.FilesOrDirectory ->
  Conduit.ConduitT i (Path.Path Path.Abs Path.File) m ()
fileOrDirectoryC (Options.FDFiles (Options.Files {..})) = do
  absFiles <- traverse Path.makeAbsolute fiFiles
  Conduit.yieldMany absFiles
fileOrDirectoryC (Options.FDDirectory (Options.Directory {..})) = do
  absDir <- Path.makeAbsolute diPath
  Conduit.sourceDirectoryDeep False (Path.toFilePath absDir)
    .| Conduit.filterC
      (\filename -> any (`Text.isSuffixOf` fromString filename) diExtensions)
    .| Conduit.mapMC Path.parseAbsFile

albumC ::
  (Monad m) =>
  Conduit.ConduitT AudioTrack.AudioTrack Album.Album m ()
albumC = clusterC Album.mkAlbum Album.addTrack

artistC ::
  (Monad m) =>
  Conduit.ConduitT Album.Album Artist.Artist m ()
artistC = clusterC Artist.mkArtist Artist.addAlbum

-- | Cluster incoming items into groups using the provided 'mk' and 'add'
-- functions
clusterC ::
  (Monad m) =>
  (NonEmpty i -> Maybe o) ->
  (i -> o -> Maybe o) ->
  Conduit.ConduitT i o m ()
clusterC mk add = loop Nothing
  where
    loop Nothing =
      Conduit.await >>= \case
        Nothing -> pure ()
        -- Create a new cluster
        Just item -> loop $ mk $ item :| []
    loop (Just cluster) =
      Conduit.await >>= \case
        Nothing -> Conduit.yield cluster
        Just item -> case add item cluster of
          Just newCluster -> loop $ Just newCluster
          Nothing -> do
            Conduit.yield cluster
            loop $ mk $ item :| []
