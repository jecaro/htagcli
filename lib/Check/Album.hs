module Check.Album (Check (..), Error (..), check, render) where

import AudioTrack qualified
import Data.Text qualified as Text
import Path ((</>))
import Path qualified
import Path.IO qualified as Path
import "extra" Data.List.NonEmpty.Extra qualified as NonEmpty

data Check
  = HaveCover (NonEmpty (Path.Path Path.Rel Path.File))
  | InSameDir
  deriving (Eq, Show)

data Error
  = NotInSameDir
  | MissingCover (Path.Path Path.Abs Path.Dir)
  deriving (Eq, Show)

render :: Error -> Text
render NotInSameDir =
  "Audio tracks are not all in the same directory"
render (MissingCover directory) =
  "Missing cover in directory: " <> Text.pack (Path.toFilePath directory)

getDirectories ::
  NonEmpty AudioTrack.AudioTrack -> NonEmpty (Path.Path Path.Abs Path.Dir)
getDirectories tracks =
  NonEmpty.nubOrd $ Path.parent . AudioTrack.atFile <$> tracks

check ::
  (MonadIO m) =>
  Check ->
  NonEmpty AudioTrack.AudioTrack ->
  m (Either Error ())
check InSameDir tracks
  | length (getDirectories tracks) == 1 = pure $ Right ()
  | otherwise = pure $ Left NotInSameDir
check (HaveCover coverFilenames) tracks
  | dir :| [] <- getDirectories tracks = do
      let absFiles = (dir </>) <$> coverFilenames
      ifM
        (anyM Path.doesFileExist absFiles)
        (pure $ Right ())
        (pure $ Left (MissingCover dir))
  | otherwise = pure $ Left NotInSameDir
