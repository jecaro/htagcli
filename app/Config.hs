{-# LANGUAGE TemplateHaskell #-}

module Config (Error (..), readConfig, checks, render) where

import Check qualified
import Data.Text qualified as Text
import GHC.IO.Exception qualified as Exception
import Path ((</>))
import Path qualified
import Path.IO qualified as Path
import Pattern qualified
import System.IO.Error qualified as Error
import Tag qualified
import Text.Megaparsec qualified as Megaparsec
import Toml ((.=))
import Toml qualified
import Toml.Extra qualified as Toml
import UnliftIO.Exception qualified as Exception
import Validation qualified

data Error
  = ErToml Text.Text
  | ErNotFound Exception.IOException
  | ErUnicode UnicodeException
  deriving (Show)

instance Exception.Exception Error

data Config = Config
  { coFilename :: Filename,
    coChecks :: Checks
  }
  deriving (Show)

-- | General filename configuration, to be used by the check command and the
-- fix-files command in the future.
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
    chGenreAmong :: Maybe (NonEmpty Text.Text),
    -- | If True, the check is enabled, the padding optionally overrides the
    -- one given in the formatting section. This way it is possible to ignore
    -- the padding when checking the filename and still have it when fixing it.
    chFilenameMatches :: (Bool, Maybe Pattern.Padding)
  }
  deriving (Show)

checks :: Config -> [Check.Check]
checks (Config {coFilename = Filename {..}, coChecks = Checks {..}}) =
  catMaybes
    [ Check.TagsExist <$> chTags,
      Check.GenreAmong <$> chGenreAmong,
      Check.FilenameMatches fiPattern
        <$> withPadding fiFormatting chFilenameMatches
    ]
  where
    withPadding ::
      Pattern.Formatting ->
      (Bool, Maybe Pattern.Padding) ->
      Maybe Pattern.Formatting
    withPadding _ (False, _) = Nothing
    -- Only return a Just when the check is enabled
    withPadding formatting (True, mbPadding) =
      Just $ maybe formatting (setPadding formatting) mbPadding
    setPadding formatting padding =
      formatting {Pattern.foPadTrackNumbers = padding}

render :: Error -> Text.Text
render (ErToml err) = "TOML error: \n" <> err
render (ErNotFound err) =
  "Config file not found: " <> maybe mempty fromString (Error.ioeGetFileName err)
render (ErUnicode err) = "Unicode error: " <> show err

readConfig :: IO Config
readConfig = do
  configFile <- getFileInConfigDir $(Path.mkRelFile "htagcli.toml")
  bytestring <-
    Exception.mapExceptionM ErNotFound $
      readFileBS $
        Path.toFilePath configFile
  text <- Exception.fromEither $ first ErUnicode $ decodeUtf8Strict bytestring
  Exception.fromEither $ decode' configC text

-- | Get a path from a file in the config directory
getFileInConfigDir ::
  (MonadIO m) => Path.Path Path.Rel t -> m (Path.Path Path.Abs t)
getFileInConfigDir file = flip (</>) file <$> getConfigDir

-- | Get the configuration directory
getConfigDir :: (MonadIO m) => m (Path.Path Path.Abs Path.Dir)
getConfigDir = Path.getXdgDir Path.XdgConfig $ Just $(Path.mkRelDir "htagcli")

-- | Variant of 'Toml.decode' that returns our custom 'Error' type
decode' :: Toml.TomlCodec c -> Text -> Either Error c
decode' codec content =
  first (ErToml . Toml.prettyTomlDecodeErrors) $
    Toml.decode codec content

configC :: Toml.TomlCodec Config
configC =
  Config
    <$> Toml.table filenameC "filename" .= coFilename
    <*> Toml.table checksC "checks" .= coChecks

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
    <*> enableAndMaybeC (paddingC "pad_track_numbers") "check_files"
      .= chFilenameMatches

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
tagsC = Toml.arrayNonEmptyOf tagC "tags"

tagC :: Toml.TomlBiMap Tag.Tag Toml.AnyValue
tagC = Toml._TextBy Tag.asText parse
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
