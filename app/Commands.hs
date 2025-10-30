module Commands
  ( FixFilePathsOptions (..),
    display,
    edit,
    check,
    fixFilePath,
    render,
  )
where

import AudioTrack qualified
import Check qualified
import Options qualified
import Path ((</>))
import Path qualified
import Path.IO qualified as Path
import Pattern qualified
import Sound.HTagLib qualified as HTagLib
import Sound.HTagLib.Extra qualified as HTagLib
import UnliftIO.Exception qualified as Exception

newtype Error = UnableToFormatFile (Path.Path Path.Abs Path.File)
  deriving (Show)

instance Exception.Exception Error

render :: Error -> Text
render (UnableToFormatFile file) = "Unable to format file: " <> show file

display :: (MonadIO m) => Path.Path Path.Abs Path.File -> m ()
display = putTextLn . AudioTrack.asText <=< AudioTrack.getTags

edit ::
  (MonadIO m) => Options.EditOptions -> Path.Path Path.Abs Path.File -> m ()
edit Options.EditOptions {..} filename = do
  let setter =
        fold $
          catMaybes
            [ HTagLib.titleSetter <$> eoTitle,
              HTagLib.artistSetter <$> eoArtist,
              HTagLib.albumSetter <$> eoAlbum,
              HTagLib.albumArtistSetter <$> eoAlbumArtist,
              HTagLib.genreSetter <$> eoGenre,
              toSetter HTagLib.yearSetter eoYear,
              toSetter HTagLib.trackNumberSetter eoTrack
            ]
  HTagLib.setTags (Path.toFilePath filename) Nothing setter
  where
    toSetter _ Nothing = Nothing
    toSetter setter (Just Options.Remove) = Just $ setter Nothing
    toSetter setter (Just (Options.Set v)) = Just . setter $ Just v

check ::
  (MonadIO m) => NonEmpty Check.Check -> Path.Path Path.Abs Path.File -> m ()
check checks filename = do
  track <- AudioTrack.getTags filename
  traverse_ (checkPrintError filename track) checks
  where
    checkPrintError file track check' =
      whenLeft_ (Check.check check' track) $ \err ->
        putTextLn $ fromString (Path.toFilePath file) <> ": " <> Check.render err

data FixFilePathsOptions = FixFilePathsOptions
  { fiDryRun :: Bool,
    fiBaseDirectory :: Path.Path Path.Abs Path.Dir,
    fiFormatting :: Pattern.Formatting,
    fiPattern :: Pattern.Pattern
  }
  deriving (Show)

fixFilePath ::
  (MonadIO m) =>
  FixFilePathsOptions ->
  Path.Path Path.Abs Path.File ->
  m ()
fixFilePath FixFilePathsOptions {..} fromFile =
  do
    track <- AudioTrack.getTags fromFile
    toFile <-
      Exception.fromEither $
        maybeToRight (UnableToFormatFile fromFile) $
          Pattern.toPath fiFormatting track fiPattern
    let toFileAbs = fiBaseDirectory </> toFile
    when (toFileAbs /= fromFile) $ do
      putTextLn $
        "Moving: " <> show fromFile <> " to " <> show toFileAbs
      when fiDryRun $ do
        Path.createDirIfMissing True (Path.parent toFileAbs)
        Path.renameFile fromFile toFileAbs
