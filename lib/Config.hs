{-# LANGUAGE TemplateHaskell #-}

module Config
  ( Error (..),
    errorToText,
    Config (..),
    Filename (..),
    readConfig,
    createConfig,
    checks,
    defaultConfigContent,
    parseByteString,
  )
where

import Check.Album qualified as Album
import Check.Artist qualified as Artist
import Check.File qualified as File
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
    chTags :: Maybe (NonEmpty Tag.Tag),
    -- | If Nothing, the check is disabled
    chGenreAmong :: Maybe (NonEmpty Text),
    -- | If True, the check is enabled, the padding optionally overrides the
    -- one given in the formatting section. This way it is possible to ignore
    -- the padding when checking the filename and still have it when fixing it.
    chFilenameMatches :: Bool,
    -- | The album have a cover file with one of the given names
    chHaveCover :: Maybe (NonEmpty (Path.Path Path.Rel Path.File)),
    -- | All the audio tracks of the album are in the same directory
    chAlbumInSameDir :: Bool,
    -- | All the audio tracks of the album have the same value for the given
    -- tags
    chAlbumSameTags :: Maybe (NonEmpty Tag.Tag),
    -- | The tracks of the album have sequential track numbers
    chAlbumTracksSequential :: Bool,
    -- | All the tracks from the artist have the same genre
    chArtistSameGenre :: Bool
  }
  deriving (Show)

fileChecks :: Config -> [File.Check]
fileChecks (Config {coFilename = Filename {..}, coChecks = Checks {..}}) =
  catMaybes
    [ File.TagsExist <$> chTags,
      File.GenreAmong <$> chGenreAmong,
      if not chFilenameMatches
        then Nothing
        else Just $ File.FilenameMatches fiPattern fiFormatting
    ]

albumChecks :: Config -> [Album.Check]
albumChecks (Config {coChecks = Checks {..}}) =
  catMaybes
    [ Album.HaveCover <$> chHaveCover,
      guarded (const chAlbumInSameDir) Album.InSameDir,
      Album.SameTag <$> chAlbumSameTags,
      guarded (const chAlbumTracksSequential) Album.TracksSequential
    ]

artistCheck :: Config -> Maybe Artist.Check
artistCheck (Config {coChecks = Checks {..}}) =
  guarded (const chArtistSameGenre) Artist.SameGenre

checks :: Config -> ([File.Check], [Album.Check], Maybe Artist.Check)
checks config = (fileChecks config, albumChecks config, artistCheck config)

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
    <$> maybeValidatedC "check_tags" tagsC chTags
    <*> maybeValidatedC "check_genre" genreAmongC chGenreAmong
    <*> checkFilesC .= chFilenameMatches
    <*> maybeValidatedC "check_cover" checkCoverC chHaveCover
    <*> checkAlbumInSameDirC .= chAlbumInSameDir
    <*> maybeValidatedC "check_album_tags" checkAlbumSameTagsC chAlbumSameTags
    <*> checkAlbumTracksSequentialC .= chAlbumTracksSequential
    <*> checkArtistSameGenreC .= chArtistSameGenre
  where
    checkFilesC = Toml.table (Toml.bool "enable") "check_files"
    checkCoverC = Toml.arrayNonEmptyOf relFileB "cover_filename"
    checkAlbumInSameDirC = Toml.table (Toml.bool "enable") "check_album_in_same_dir"
    checkAlbumSameTagsC = Toml.arrayNonEmptyOf tagB "tags"
    checkAlbumTracksSequentialC =
      Toml.table (Toml.bool "enable") "check_album_tracks_sequential"
    checkArtistSameGenreC =
      Toml.table (Toml.bool "enable") "check_artist_same_genre"

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

tagsC :: Toml.TomlCodec (NonEmpty Tag.Tag)
tagsC = Toml.arrayNonEmptyOf tagB "tags"

tagB :: Toml.TomlBiMap Tag.Tag Toml.AnyValue
tagB = Toml._TextBy Tag.asText parse
  where
    parse :: Text -> Either Text Tag.Tag
    parse text = case Megaparsec.parseMaybe Tag.parser text of
      Just tag -> Right tag
      Nothing -> Left $ "Invalid tag: " <> text

genreAmongC :: Toml.TomlCodec (NonEmpty Text)
genreAmongC = Toml.arrayNonEmptyOf Toml._Text "genre_among"

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
