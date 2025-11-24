{-# LANGUAGE TemplateHaskell #-}

module Config
  ( Error (..),
    errorToText,
    Config (..),
    Checks (..),
    Filename (..),
    haveChecks,
    readConfig,
    createConfig,
    factorChecks,
    factorChecks',
    defaultConfigContent,
    parseByteString,
  )
where

import Check.Album qualified as Album
import Check.Artist qualified as Artist
import Check.Track qualified as Track
import Data.ByteString qualified as ByteString
import Data.FileEmbed qualified as FileEmbed
import Data.Text qualified as Text
import GHC.IO.Exception qualified as Exception
import Path ((</>))
import Path qualified
import Path.IO qualified as Path
import Pattern qualified
import System.IO qualified as System
import System.IO.Error qualified as Error
import Tag qualified
import Text.Megaparsec qualified as Megaparsec
import Toml ((.=))
import Toml qualified
import Toml.Extra qualified as Toml
import UnliftIO.Exception qualified as Exception
import Validation qualified

data Error
  = ErToml Text
  | ErNotFound Exception.IOException
  | ErUnicode UnicodeException
  | ErConfigFileExists
  deriving (Show)

instance Exception.Exception Error

data Config = Config
  { coFilename :: Filename,
    coFixPaths :: Path.Path Path.Abs Path.Dir,
    coChecks :: Checks
  }
  deriving (Show)

-- | General filename configuration, to be used by the check command and the
-- fix-files command
data Filename = Filename
  { fiPattern :: Pattern.Pattern,
    fiFormatting :: Pattern.Formatting
  }
  deriving (Show)

-- | List of the checks to perform
data Checks = Checks
  { -- | If Nothing, the check is disabled
    chTrackTags :: Maybe (NonEmpty Tag.Tag),
    -- | If Nothing, the check is disabled
    chTrackGenreAmong :: Maybe (NonEmpty Text),
    -- | If True, the check is enabled, the padding optionally overrides the
    -- one given in the formatting section. This way it is possible to ignore
    -- the padding when checking the filename and still have it when fixing it.
    chTrackFilename :: Bool,
    -- | The album have a cover file with one of the given names
    chAlbumHaveCover :: Maybe (NonEmpty (Path.Path Path.Rel Path.File)),
    -- | All the audio tracks of the album are in the same directory
    chAlbumSameDir :: Bool,
    -- | All the audio tracks of the album have the same value for the given
    -- tags
    chAlbumSameTags :: Maybe (NonEmpty Tag.Tag),
    -- | The tracks of the album have sequential track numbers
    chAlbumTracksSequential :: Bool,
    -- | All the tracks from the artist have the same genre
    chArtistSameGenre :: Bool
  }
  deriving (Show)

haveChecks :: Checks -> Bool
haveChecks (Checks {..}) =
  or
    [ isJust chTrackTags,
      isJust chTrackGenreAmong,
      chTrackFilename,
      isJust chAlbumHaveCover,
      chAlbumSameDir,
      isJust chAlbumSameTags,
      chAlbumTracksSequential,
      chArtistSameGenre
    ]

trackChecks :: Pattern.Pattern -> Pattern.Formatting -> Checks -> [Track.Check]
trackChecks pattern formatting Checks {..} =
  catMaybes
    [ Track.TagsExist <$> chTrackTags,
      Track.GenreAmong <$> chTrackGenreAmong,
      if not chTrackFilename
        then Nothing
        else Just $ Track.FilenameMatches pattern formatting
    ]

albumChecks :: Checks -> [Album.Check]
albumChecks (Checks {..}) =
  catMaybes
    [ Album.HaveCover <$> chAlbumHaveCover,
      guarded (const chAlbumSameDir) Album.InSameDir,
      Album.SameTags <$> chAlbumSameTags,
      guarded (const chAlbumTracksSequential) Album.TracksSequential
    ]

artistCheck :: Checks -> Maybe Artist.Check
artistCheck (Checks {..}) =
  guarded (const chArtistSameGenre) Artist.SameGenre

factorChecks :: Config -> ([Track.Check], [Album.Check], Maybe Artist.Check)
factorChecks Config {coFilename = Filename {..}, ..} =
  factorChecks' fiPattern fiFormatting coChecks

factorChecks' ::
  Pattern.Pattern ->
  Pattern.Formatting ->
  Checks ->
  ([Track.Check], [Album.Check], Maybe Artist.Check)
factorChecks' pattern formatting checks =
  ( trackChecks pattern formatting checks,
    albumChecks checks,
    artistCheck checks
  )

errorToText :: Error -> Text
errorToText (ErToml err) = "TOML error: \n" <> err
errorToText (ErNotFound err) =
  unlines
    [ "Config file not found: "
        <> maybe mempty fromString (Error.ioeGetFileName err),
      "Please create a config file using the 'create-config' command."
    ]
errorToText (ErUnicode err) = "Unicode error: " <> show err
errorToText ErConfigFileExists = "Config file already exists, not overwriting it"

defaultConfigContent :: ByteString.ByteString
defaultConfigContent = $(FileEmbed.embedFileRelative "data/htagcli.toml")

readConfig :: IO Config
readConfig = do
  configFile <- getConfigFile
  bytestring <-
    Exception.mapExceptionM ErNotFound $ readFileBS $ Path.toFilePath configFile
  parseByteString bytestring

parseByteString :: ByteString.ByteString -> IO Config
parseByteString bytestring = do
  text <- Exception.fromEither $ first ErUnicode $ decodeUtf8Strict bytestring
  Exception.fromEither $ decode' configC text

-- | Get a path from a file in the config directory
getFileInConfigDir ::
  (MonadIO m) => Path.Path Path.Rel t -> m (Path.Path Path.Abs t)
getFileInConfigDir file = flip (</>) file <$> getConfigDir

getConfigFile ::
  (MonadIO m) => m (Path.Path Path.Abs Path.File)
getConfigFile = getFileInConfigDir $(Path.mkRelFile "htagcli.toml")

-- | Get the configuration directory
getConfigDir :: (MonadIO m) => m (Path.Path Path.Abs Path.Dir)
getConfigDir = Path.getXdgDir Path.XdgConfig $ Just $(Path.mkRelDir "htagcli")

createConfig :: IO ()
createConfig = do
  getConfigDir >>= Path.ensureDir
  configFile <- getConfigFile
  whenM (Path.doesFileExist configFile) $
    Exception.throwIO ErConfigFileExists
  System.withFile (Path.toFilePath configFile) System.WriteMode $
    flip ByteString.hPut defaultConfigContent
  putTextLn $
    "Created default config file at: " <> toText (Path.toFilePath configFile)

-- | Variant of 'Toml.decode' that returns our custom 'Error' type
decode' :: Toml.TomlCodec c -> Text -> Either Error c
decode' codec content =
  first (ErToml . Toml.prettyTomlDecodeErrors) $
    Toml.decode codec content

configC :: Toml.TomlCodec Config
configC =
  Config
    <$> Toml.table filenameC "filename" .= coFilename
    <*> Toml.table (absDirC "base_dir") "fix_paths" .= coFixPaths
    <*> Toml.table checksC "checks" .= coChecks

absDirC :: Toml.Key -> Toml.TomlCodec (Path.Path Path.Abs Path.Dir)
absDirC =
  Toml.textBy (toText . Path.toFilePath) parse
  where
    parse :: Text -> Either Text (Path.Path Path.Abs Path.Dir)
    parse text = case Path.parseAbsDir (toString text) of
      Nothing -> Left "Invalid absolute directory path"
      Just directory -> Right directory

relFileB :: Toml.TomlBiMap (Path.Path Path.Rel Path.File) Toml.AnyValue
relFileB = Toml._TextBy (toText . Path.toFilePath) parse
  where
    parse :: Text -> Either Text (Path.Path Path.Rel Path.File)
    parse text = case Path.parseRelFile (toString text) of
      Nothing -> Left "Invalid relative file path"
      Just file -> Right file

filenameC :: Toml.TomlCodec Filename
filenameC =
  Filename
    <$> patternC .= fiPattern
    <*> formattingC .= fiFormatting

checksC :: Toml.TomlCodec Checks
checksC =
  Checks
    <$> maybeValidatedC "track_tags" tagsC chTrackTags
    <*> maybeValidatedC "track_genre" amongC chTrackGenreAmong
    <*> trackFilenameC .= chTrackFilename
    <*> maybeValidatedC "album_cover" filenamesC chAlbumHaveCover
    <*> albumSameDirC .= chAlbumSameDir
    <*> maybeValidatedC "album_tags" tagsC chAlbumSameTags
    <*> albumTracksSequentialC .= chAlbumTracksSequential
    <*> artistSameGenreC .= chArtistSameGenre
  where
    trackFilenameC = Toml.table (Toml.bool "enable") "track_filename"
    filenamesC = Toml.arrayNonEmptyOf relFileB "filenames"
    albumSameDirC = Toml.table (Toml.bool "enable") "album_same_dir"
    tagsC = Toml.arrayNonEmptyOf tagB "tags"
    albumTracksSequentialC =
      Toml.table (Toml.bool "enable") "album_tracks_sequential"
    artistSameGenreC = Toml.table (Toml.bool "enable") "artist_same_genre"
    amongC = Toml.arrayNonEmptyOf Toml._Text "among"

-- | Unwrap the Maybe value according to the enable flag.
maybeValidatedC ::
  Toml.Key ->
  Toml.TomlCodec a ->
  (object -> Maybe a) ->
  Toml.Codec object (Maybe a)
maybeValidatedC key codec getter =
  (unwrap <$> enableAndMaybeValidatedC codec key) .= wrap getter
  where
    unwrap (True, Just a) = Just a
    unwrap _ = Nothing
    wrap :: (a -> Maybe b) -> a -> (Bool, Maybe b)
    wrap getter' a
      | Just b <- getter' a = (True, Just b)
      | otherwise = (False, Nothing)

-- | From a Codec for a value, create a Codec for an optional value that is
-- defined by a table with an "enable" boolean and the value itself. If the
-- enable flag is true, the value must be present.
enableAndMaybeValidatedC ::
  Toml.TomlCodec a ->
  Toml.Key ->
  Toml.TomlCodec (Bool, Maybe a)
enableAndMaybeValidatedC codec key =
  Toml.thenValidate validate $ enableAndMaybeC codec key
  where
    validate (True, Nothing) =
      Validation.Failure
        [Toml.BiMapError key $ Toml.ArbitraryError "Enabled but no value"]
    validate v = Validation.Success v

-- | Codec for a table with an optional value and an "enable" boolean
enableAndMaybeC :: Toml.TomlCodec a -> Toml.Key -> Toml.TomlCodec (Bool, Maybe a)
enableAndMaybeC codec =
  Toml.table $
    Toml.pair
      (Toml.bool "enable")
      (Toml.dioptional codec)

tagB :: Toml.TomlBiMap Tag.Tag Toml.AnyValue
tagB = Toml._TextBy Tag.asText parse
  where
    parse :: Text -> Either Text Tag.Tag
    parse text = case Megaparsec.parseMaybe Tag.parser text of
      Just tag -> Right tag
      Nothing -> Left $ "Invalid tag: " <> text

patternC :: Toml.TomlCodec Pattern.Pattern
patternC = Toml.textBy Pattern.asText parse "filename_matches"
  where
    parse :: Text -> Either Text Pattern.Pattern
    parse text =
      first (toText . Megaparsec.errorBundlePretty) $
        Megaparsec.parse Pattern.parser "" text

formattingC :: Toml.TomlCodec Pattern.Formatting
formattingC =
  Pattern.Formatting
    <$> ( Pattern.addSlashIfNeeded
            <$> Toml.list charAndCharActionC "unwanted" .= Pattern.foCharActions
        )
    <*> paddingC "pad_track_numbers" .= Pattern.foPadTrackNumbers
    <*> paddingC "pad_disc_numbers" .= Pattern.foPadDiscNumbers

charAndCharActionC :: Toml.TomlCodec (Char, Pattern.CharAction)
charAndCharActionC = Toml.pair (charC "char") (charActionC "action")

charC :: Toml.Key -> Toml.TomlCodec Char
charC = Toml.textBy Text.singleton parseChar
  where
    parseChar :: Text -> Either Text Char
    parseChar text
      | Just (c, rest) <- Text.uncons text,
        Text.null rest =
          Right c
      | otherwise = Left "Should be a single character"

charActionC :: Toml.Key -> Toml.TomlCodec Pattern.CharAction
charActionC = Toml.textBy Pattern.charActionAsText parse
  where
    parse :: Text -> Either Text Pattern.CharAction
    parse text = case Megaparsec.parseMaybe Pattern.charActionParser text of
      Just charAction -> Right charAction
      Nothing -> Left $ "Invalid ation: " <> text

paddingC :: Toml.Key -> Toml.TomlCodec Pattern.Padding
paddingC = Toml.textBy Pattern.paddingAsText Pattern.parsePadding
