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
import System.FilePath qualified as FilePath

runConduitWithProgress ::
  Options.Files ->
  Conduit.ConduitT
    (Path.Path Path.Abs Path.File)
    Void
    (Conduit.ResourceT IO)
    a ->
  IO a
runConduitWithProgress = Progress.connectWithProgress . filesC

filesC ::
  (Conduit.MonadResource m, Conduit.MonadThrow m) =>
  Options.Files ->
  Conduit.ConduitT i (Path.Path Path.Abs Path.File) m ()
filesC Options.Files {..} = do
  flip foldMap fiPaths $ \text -> do
    if FilePath.hasExtension $ toString text
      then do
        file <- Path.resolveFile' $ toString text
        Conduit.yield file
      else do
        absDir <- Path.resolveDir' $ toString text
        Conduit.sourceDirectoryDeep False (Path.toFilePath absDir)
          .| Conduit.filterC
            ( \filename ->
                any (`Text.isSuffixOf` fromString filename) fiExtensions
            )
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
