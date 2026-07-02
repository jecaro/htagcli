{-# LANGUAGE NoMonoLocalBinds #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Commands.FileSystem
  ( FileSystem,
    withRealFileSystem,
    withOverlayFileSystem,
    doesFileExist,
    ensureDir,
    renameFile,
    isDirEmpty,
    removeDir,
    printLine,
    removeDirAndParentsIfEmpty,
  )
where

import Bluefin.Compound qualified as Bluefin
import Bluefin.Eff ((:&), (:>))
import Bluefin.Eff qualified as Bluefin
import Bluefin.IO qualified as Bluefin
import Bluefin.State qualified as Bluefin
import Data.Set qualified as Set
import Path qualified
import Path.IO qualified as Path

data FileSystem (es :: Bluefin.Effects) = MkFileSystem
  { fiDoesFileExistImpl ::
      forall e.
      Path.Path Path.Abs Path.File -> Bluefin.Eff (e :& es) Bool,
    fiEnsureDirImpl ::
      forall e.
      Path.Path Path.Abs Path.Dir -> Bluefin.Eff (e :& es) (),
    fiRenameFileImpl ::
      forall e.
      Path.Path Path.Abs Path.File ->
      Path.Path Path.Abs Path.File ->
      Bluefin.Eff (e :& es) (),
    fiIsDirEmptyImpl ::
      forall e.
      Path.Path Path.Abs Path.Dir -> Bluefin.Eff (e :& es) Bool,
    fiRemoveDirImpl ::
      forall e.
      Path.Path Path.Abs Path.Dir -> Bluefin.Eff (e :& es) (),
    fiPrintLineImpl ::
      forall e.
      Text -> Bluefin.Eff (e :& es) ()
  }

instance Bluefin.Handle FileSystem where
  mapHandle MkFileSystem {..} =
    MkFileSystem
      { fiDoesFileExistImpl = Bluefin.useImplUnder . fiDoesFileExistImpl,
        fiEnsureDirImpl = Bluefin.useImplUnder . fiEnsureDirImpl,
        fiRenameFileImpl =
          \from to -> Bluefin.useImplUnder $ fiRenameFileImpl from to,
        fiIsDirEmptyImpl = Bluefin.useImplUnder . fiIsDirEmptyImpl,
        fiRemoveDirImpl = Bluefin.useImplUnder . fiRemoveDirImpl,
        fiPrintLineImpl = Bluefin.useImplUnder . fiPrintLineImpl
      }

doesFileExist ::
  (e :> es) =>
  FileSystem e ->
  Path.Path Path.Abs Path.File ->
  Bluefin.Eff es Bool
doesFileExist fs = Bluefin.makeOp . fiDoesFileExistImpl (Bluefin.mapHandle fs)

ensureDir ::
  (e :> es) =>
  FileSystem e ->
  Path.Path Path.Abs Path.Dir ->
  Bluefin.Eff es ()
ensureDir fs = Bluefin.makeOp . fiEnsureDirImpl (Bluefin.mapHandle fs)

renameFile ::
  (e :> es) =>
  FileSystem e ->
  Path.Path Path.Abs Path.File ->
  Path.Path Path.Abs Path.File ->
  Bluefin.Eff es ()
renameFile fs from to =
  Bluefin.makeOp $ fiRenameFileImpl (Bluefin.mapHandle fs) from to

isDirEmpty ::
  (e :> es) =>
  FileSystem e ->
  Path.Path Path.Abs Path.Dir ->
  Bluefin.Eff es Bool
isDirEmpty fs = Bluefin.makeOp . fiIsDirEmptyImpl (Bluefin.mapHandle fs)

removeDir ::
  (e :> es) =>
  FileSystem e ->
  Path.Path Path.Abs Path.Dir ->
  Bluefin.Eff es ()
removeDir fs = Bluefin.makeOp . fiRemoveDirImpl (Bluefin.mapHandle fs)

printLine ::
  (e :> es) =>
  FileSystem e ->
  Text ->
  Bluefin.Eff es ()
printLine fs = Bluefin.makeOp . fiPrintLineImpl (Bluefin.mapHandle fs)

withRealFileSystem ::
  (io :> es) =>
  Bluefin.IOE io ->
  (forall e. FileSystem e -> Bluefin.Eff (e :& es) r) ->
  Bluefin.Eff es r
withRealFileSystem ioe action = Bluefin.useImplIn action MkFileSystem {..}
  where
    fiDoesFileExistImpl = Bluefin.effIO ioe . Path.doesFileExist
    fiEnsureDirImpl = Bluefin.effIO ioe . Path.ensureDir
    fiRenameFileImpl from to = Bluefin.effIO ioe $ Path.renameFile from to
    fiIsDirEmptyImpl dir = Bluefin.effIO ioe $ do
      (dirs, files) <- Path.listDir dir
      pure $ null dirs && null files
    fiRemoveDirImpl = Bluefin.effIO ioe . Path.removeDir
    fiPrintLineImpl = Bluefin.effIO ioe . putTextLn

data Overlay = Overlay
  { ovAdded :: Set.Set (Path.Path Path.Abs Path.File),
    ovDeleted :: Set.Set (Path.Path Path.Abs Path.File),
    ovDeletedDirs :: Set.Set (Path.Path Path.Abs Path.Dir)
  }

emptyOverlay :: Overlay
emptyOverlay = Overlay Set.empty Set.empty Set.empty

withOverlayFileSystem ::
  (io :> es) =>
  Bluefin.IOE io ->
  (forall e. FileSystem e -> Bluefin.Eff (e :& es) r) ->
  Bluefin.Eff es r
withOverlayFileSystem ioe action =
  Bluefin.evalState emptyOverlay $ \st -> do
    let fiDoesFileExistImpl path = do
          Overlay {..} <- Bluefin.get st
          if
            | path `Set.member` ovDeleted -> pure False
            | path `Set.member` ovAdded -> pure True
            | otherwise -> Bluefin.effIO ioe $ Path.doesFileExist path
        fiEnsureDirImpl = const $ pure ()
        fiRenameFileImpl from to =
          Bluefin.modify st $ \overlay ->
            overlay
              { ovAdded = Set.insert to $ ovAdded overlay,
                ovDeleted = Set.insert from $ ovDeleted overlay
              }
        fiIsDirEmptyImpl dir = do
          (realDirs, realFiles) <- Bluefin.effIO ioe $ Path.listDir dir
          Overlay {..} <- Bluefin.get st
          let vFiles = filter (`Set.notMember` ovDeleted) realFiles
              vDirs = filter (`Set.notMember` ovDeletedDirs) realDirs
              hasAddedFiles = any ((== dir) . Path.parent) $ Set.toList ovAdded
          pure (null vFiles && null vDirs && not hasAddedFiles)
        fiRemoveDirImpl dir =
          Bluefin.modify st $ \overlay ->
            overlay
              { ovDeletedDirs = Set.insert dir $ ovDeletedDirs overlay
              }
        fiPrintLineImpl = Bluefin.effIO ioe . putTextLn

    Bluefin.useImplIn action MkFileSystem {..}

removeDirAndParentsIfEmpty ::
  (e :> es) =>
  FileSystem e ->
  Path.Path Path.Abs Path.Dir ->
  Bluefin.Eff es ()
removeDirAndParentsIfEmpty fs dir =
  whenM (isDirEmpty fs dir) $ do
    printLine fs $ fromString (Path.toFilePath dir) <> " (deleted)"
    removeDir fs dir
    let parent = Path.parent dir
    when (parent /= dir) $ removeDirAndParentsIfEmpty fs parent
