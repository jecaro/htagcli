module Path.IO.Extra (removeDirAndParentsIfEmpty) where

import Path qualified
import Path.IO qualified as Path

removeDirAndParentsIfEmpty :: (MonadIO m) => Path.Path Path.Abs Path.Dir -> m ()
removeDirAndParentsIfEmpty dir = do
  (files, dirs) <- Path.listDir dir
  when (null files && null dirs) $ do
    Path.removeDir dir
    removeDirAndParentsIfEmpty (Path.parent dir)
