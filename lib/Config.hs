{-# LANGUAGE TemplateHaskell #-}

module Config
  ( Error (..),
    errorToText,
    Config (..),
    Checks (..),
    Filename (..),
    FixPaths (..),
    AllChecks (..),
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
import Check.Disc qualified as Disc
import Check.Track qualified as Track
import Data.ByteString qualified as ByteString
import Data.FileEmbed qualified as FileEmbed
import Data.Text qualified as Text
import GHC.IO.Exception qualified as Exception
import Model.Cover qualified as Cover
import Model.Pattern qualified as Pattern
import Model.Tag qualified as Tag
import Path ((</>))
import Path qualified
import Path.IO qualified as Path
import Sound.HTagLib qualified as HTagLib
import System.IO qualified as System
import System.IO.Error qualified as Error
import TOML qualified
import Text.Megaparsec qualified as Megaparsec
import UnliftIO.Exception qualified as Exception

data Error
  = ErToml TOML.TOMLError
  | ErNotFound Exception.IOException
  | ErUnicode UnicodeException
  | ErConfigFileExists
  deriving (Show)

instance Exception.Exception Error

data FixPaths = FixPaths
  { fiBaseDir :: Path.Path Path.Abs Path.Dir,
    fiMoveCover :: Bool
  }
  deriving (Show)

data Config = Config
  { coFilename :: Filename,
    coFixPaths :: FixPaths,
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
    chTrackGenreAmong :: Maybe (NonEmpty HTagLib.Genre),
    -- | If True, the check is enabled, the padding optionally overrides the
    -- one given in the formatting section. This way it is possible to ignore
    -- the padding when checking the filename and still have it when fixing it.
    chTrackFilename :: Bool,
    -- | The disc have a cover file
    chDiscHaveCover :: Maybe Cover.Cover,
    -- | All the audio tracks of the disc are in the same directory
    chDiscSameDir :: Bool,
    -- | All the audio tracks of the disc have the same value for the given
    -- tags
    chDiscSameTags :: Maybe (NonEmpty Tag.Tag),
    -- | The tracks of the disc have sequential track numbers
    chDiscTracksSequential :: Bool,
    -- | The disc numbers of the album are sequential
    chAlbumDiscsSequential :: Bool,
    -- | All the discs of the album have the same value for the given tags
    chAlbumSameTags :: Maybe (NonEmpty Tag.Tag),
    -- | All the tracks from the artist have the same genre
    chArtistSameGenre :: Maybe Artist.Check
  }
  deriving (Show)

haveChecks :: Checks -> Bool
haveChecks (Checks {..}) =
  or
    [ isJust chTrackTags,
      isJust chTrackGenreAmong,
      chTrackFilename,
      isJust chDiscHaveCover,
      chDiscSameDir,
      isJust chDiscSameTags,
      chDiscTracksSequential,
      chAlbumDiscsSequential,
      isJust chAlbumSameTags,
      isJust chArtistSameGenre
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

discChecks :: Checks -> [Disc.Check]
discChecks (Checks {..}) =
  catMaybes
    [ Disc.HaveCover <$> chDiscHaveCover,
      guarded (const chDiscSameDir) Disc.InSameDir,
      Disc.SameTags <$> chDiscSameTags,
      guarded (const chDiscTracksSequential) Disc.TracksSequential
    ]

albumChecks :: Checks -> [Album.Check]
albumChecks (Checks {..}) =
  catMaybes
    [ guarded (const chAlbumDiscsSequential) Album.DiscsSequential,
      Album.SameTags <$> chAlbumSameTags
    ]

artistCheck :: Checks -> Maybe Artist.Check
artistCheck (Checks {..}) = chArtistSameGenre

data AllChecks = AllChecks
  { alTrack :: [Track.Check],
    alDisc :: [Disc.Check],
    alAlbum :: [Album.Check],
    alArtist :: Maybe Artist.Check
  }

factorChecks :: Config -> AllChecks
factorChecks Config {coFilename = Filename {..}, ..} =
  factorChecks' fiPattern fiFormatting coChecks

factorChecks' ::
  Pattern.Pattern ->
  Pattern.Formatting ->
  Checks ->
  AllChecks
factorChecks' pattern formatting checks =
  AllChecks
    { alTrack = trackChecks pattern formatting checks,
      alDisc = discChecks checks,
      alAlbum = albumChecks checks,
      alArtist = artistCheck checks
    }

errorToText :: Error -> Text
errorToText (ErToml err) =
  unlines
    [ "TOML error: ",
      TOML.renderTOMLError err
    ]
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
  Exception.fromEither $ first ErToml $ TOML.decodeWith configD text

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

configD :: TOML.Decoder Config
configD =
  Config
    <$> TOML.getFieldWith filenameD "filename"
    <*> TOML.getFieldWith fixPathsD "fix_paths"
    <*> TOML.getFieldWith checksD "checks"

fixPathsD :: TOML.Decoder FixPaths
fixPathsD =
  FixPaths
    <$> TOML.getFieldWith absDirD "base_dir"
    <*> TOML.getField "move_cover"

-- | Decode a string with the given parser
textD :: (Text -> Either Text a) -> TOML.Decoder a
textD parse =
  TOML.tomlDecoder
    >>= either (TOML.makeDecoder . TOML.invalidValue) pure . parse

-- | Decode a string with a megaparsec parser
parserD :: Megaparsec.Parsec Void Text a -> TOML.Decoder a
parserD parser =
  textD $
    first (toText . Megaparsec.errorBundlePretty)
      . Megaparsec.parse (parser <* Megaparsec.eof) ""

absDirD :: TOML.Decoder (Path.Path Path.Abs Path.Dir)
absDirD = textD $ \text -> case Path.parseAbsDir (toString text) of
  Nothing -> Left "Invalid absolute directory path"
  Just directory -> Right directory

relFileD :: TOML.Decoder (Path.Path Path.Rel Path.File)
relFileD = textD $ \text -> case Path.parseRelFile (toString text) of
  Nothing -> Left "Invalid relative file path"
  Just file -> Right file

nonEmptyD :: TOML.Decoder a -> TOML.Decoder (NonEmpty a)
nonEmptyD decoder =
  TOML.getArrayOf decoder
    >>= maybe (TOML.makeDecoder $ TOML.invalidValue "Got empty list") pure
      . nonEmpty

filenameD :: TOML.Decoder Filename
filenameD =
  Filename
    <$> TOML.getFieldWith patternD "filename_matches"
    <*> formattingD

checksD :: TOML.Decoder Checks
checksD =
  Checks
    <$> enabledWith "track_tags" tagsD
    <*> enabledWith "track_genre" (TOML.getFieldWith (nonEmptyD genreD) "among")
    <*> enabled "track_filename"
    <*> enabledWith "disc_cover" discHaveCoverD
    <*> enabled "disc_same_dir"
    <*> enabledWith "disc_tags" tagsD
    <*> enabled "disc_tracks_sequential"
    <*> enabled "album_discs_sequential"
    <*> enabledWith "album_tags" tagsD
    <*> enabledWith "artist_same_genre" artistSameGenreD
  where
    tagsD = TOML.getFieldWith (nonEmptyD tagD) "tags"
    artistSameGenreD =
      Artist.SameGenre . fromList . fromMaybe []
        <$> TOML.getFieldOptWith (TOML.getArrayOf exceptD) "except"
    exceptD =
      (,)
        <$> TOML.getField "artist"
        <*> TOML.getFieldWith (nonEmptyD genreD) "genres"
    discHaveCoverD =
      Cover.Cover
        <$> TOML.getFieldWith (nonEmptyD relFileD) "filenames"
        <*> TOML.getFieldOptWith sizeD "min_size"
        <*> TOML.getFieldOptWith sizeD "max_size"
    sizeD =
      Cover.Size
        <$> TOML.getField "width"
        <*> TOML.getField "height"

-- | Read the "enable" flag of the given table
enabled :: Text -> TOML.Decoder Bool
enabled key = TOML.getFields [key, "enable"]

-- | Decode the given table only if its "enable" flag is true
enabledWith :: Text -> TOML.Decoder a -> TOML.Decoder (Maybe a)
enabledWith key decoder = do
  isEnabled <- enabled key
  if isEnabled
    then Just <$> TOML.getFieldWith decoder key
    else pure Nothing

genreD :: TOML.Decoder HTagLib.Genre
genreD = HTagLib.mkGenre <$> TOML.tomlDecoder

tagD :: TOML.Decoder Tag.Tag
tagD = parserD Tag.parser

patternD :: TOML.Decoder Pattern.Pattern
patternD = parserD Pattern.parser

formattingD :: TOML.Decoder Pattern.Formatting
formattingD =
  Pattern.Formatting
    <$> charActionsD
    <*> TOML.getFieldWith paddingD "pad_track_numbers"
    <*> TOML.getFieldWith paddingD "pad_disc_numbers"
    <*> TOML.getField "placeholder_max_length"
  where
    -- Optional list, always removing '/' to avoid creating directories
    charActionsD =
      Pattern.addSlashIfNeeded . fromMaybe []
        <$> TOML.getFieldOptWith
          (TOML.getArrayOf charAndCharActionD)
          "unwanted"

charAndCharActionD :: TOML.Decoder (Char, Pattern.CharAction)
charAndCharActionD =
  (,)
    <$> TOML.getFieldWith charD "char"
    <*> TOML.getFieldWith charActionD "action"

charD :: TOML.Decoder Char
charD = textD $ \text -> case Text.uncons text of
  Just (c, rest) | Text.null rest -> Right c
  _ -> Left "Should be a single character"

charActionD :: TOML.Decoder Pattern.CharAction
charActionD = parserD Pattern.charActionParser

paddingD :: TOML.Decoder Pattern.Padding
paddingD = textD Pattern.parsePadding
