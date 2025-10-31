module Check.Album (Check (..), Error (..), check, render) where

import AudioTrack qualified
import Data.Text qualified as Text
import Path ((</>))
import Path qualified
import Path.IO qualified as Path
import "extra" Data.List.NonEmpty.Extra qualified as NonEmpty

data Check
  = HaveCover (Path.Path Path.Rel Path.File)
  | InSameDir
  deriving (Eq, Show)

data Error
  = NotInSameDir
  | MissingCover (Path.Path Path.Abs Path.File)
  deriving (Eq, Show)

render :: Error -> Text
render NotInSameDir =
  "Audio tracks are not all in the same directory"
render (MissingCover coverFilename) =
  "Missing cover file: " <> Text.pack (Path.toFilePath coverFilename)

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
check (HaveCover coverFilename) tracks
  | dir :| [] <- getDirectories tracks = do
      let absFile = dir </> coverFilename
      ifM
        (Path.doesFileExist absFile)
        (pure $ Right ())
        (pure $ Left (MissingCover absFile))
  | otherwise = pure $ Left NotInSameDir
