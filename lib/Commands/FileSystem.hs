module Commands.FileSystem
  ( FileSystem (..),
    mkReal,
    mkOverlay,
    removeDirAndParentsIfEmpty,
  )
where

import Data.Set qualified as Set
import Path qualified
import Path.IO qualified as Path
import UnliftIO.IORef qualified as IORef

data FileSystem m = FileSystem
  { fiDoesFileExist :: Path.Path Path.Abs Path.File -> m Bool,
    fiEnsureDir :: Path.Path Path.Abs Path.Dir -> m (),
    fiRenameFile ::
      Path.Path Path.Abs Path.File ->
      Path.Path Path.Abs Path.File ->
      m (),
    fiIsDirEmpty :: Path.Path Path.Abs Path.Dir -> m Bool,
    fiRemoveDir :: Path.Path Path.Abs Path.Dir -> m ()
  }

removeDirAndParentsIfEmpty ::
  (MonadIO m) =>
  FileSystem m ->
  Path.Path Path.Abs Path.Dir ->
  m ()
removeDirAndParentsIfEmpty fs@FileSystem {..} dir =
  whenM (fiIsDirEmpty dir) $ do
    putTextLn $ fromString (Path.toFilePath dir) <> " (deleted)"
    fiRemoveDir dir
    let parent = Path.parent dir
    when (parent /= dir) $ removeDirAndParentsIfEmpty fs parent

mkReal :: (MonadIO m) => FileSystem m
mkReal =
  FileSystem
    { fiDoesFileExist = Path.doesFileExist,
      fiEnsureDir = Path.ensureDir,
      fiRenameFile = Path.renameFile,
      fiIsDirEmpty = \dir -> do
        (dirs, files) <- Path.listDir dir
        pure $ null dirs && null files,
      fiRemoveDir = Path.removeDir
    }

data Overlay = Overlay
  { ovAdded :: Set.Set (Path.Path Path.Abs Path.File),
    ovDeleted :: Set.Set (Path.Path Path.Abs Path.File),
    ovDeletedDirs :: Set.Set (Path.Path Path.Abs Path.Dir)
  }

mkOverlay :: (MonadIO m) => IO (FileSystem m)
mkOverlay = overlayFs <$> IORef.newIORef emptyOverlay
  where
    emptyOverlay =
      Overlay
        { ovAdded = Set.empty,
          ovDeleted = Set.empty,
          ovDeletedDirs = Set.empty
        }

overlayFs :: (MonadIO m) => IORef.IORef Overlay -> FileSystem m
overlayFs ref = FileSystem {..}
  where
    fiDoesFileExist path = do
      Overlay {..} <- IORef.readIORef ref
      if
        | path `Set.member` ovDeleted -> pure False
        | path `Set.member` ovAdded -> pure True
        | otherwise -> liftIO $ Path.doesFileExist path
    fiEnsureDir _ = pure ()
    fiRenameFile from to =
      IORef.modifyIORef ref $ \overlay ->
        overlay
          { ovAdded = Set.insert to $ ovAdded overlay,
            ovDeleted = Set.insert from $ ovDeleted overlay
          }
    fiIsDirEmpty dir = do
      (realDirs, realFiles) <- liftIO $ Path.listDir dir
      Overlay {..} <- IORef.readIORef ref
      let vFiles = filter (`Set.notMember` ovDeleted) realFiles
          vDirs = filter (`Set.notMember` ovDeletedDirs) realDirs
          hasAddedFiles = any ((== dir) . Path.parent) $ Set.toList ovAdded
      pure (null vFiles && null vDirs && not hasAddedFiles)
    fiRemoveDir dir =
      IORef.modifyIORef ref $ \overlay ->
        overlay {ovDeletedDirs = Set.insert dir $ ovDeletedDirs overlay}
