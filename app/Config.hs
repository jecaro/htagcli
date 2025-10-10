{-# LANGUAGE TemplateHaskell #-}

module Config (Error (..), readChecks, render) where

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
import UnliftIO.Exception qualified as Exception

data Error
  = ErToml Text.Text
  | ErNotFound Exception.IOException
  | ErUnicode UnicodeException
  deriving (Show)

instance Exception.Exception Error

render :: Error -> Text.Text
render (ErToml err) = "TOML error: \n" <> err
render (ErNotFound err) =
  "Config file not found: " <> maybe mempty fromString (Error.ioeGetFileName err)
render (ErUnicode err) = "Unicode error: " <> show err

readChecks :: IO (NonEmpty Check.Check)
readChecks = do
  configFile <- getFileInConfigDir $(Path.mkRelFile "htagcli.toml")
  bytestring <-
    Exception.mapExceptionM ErNotFound $
      readFileBS $
        Path.toFilePath configFile
  text <- Exception.fromEither $ first ErUnicode $ decodeUtf8Strict bytestring
  Exception.fromEither $ decodeChecks text

-- | Get a path from a file in the config directory
getFileInConfigDir ::
  (MonadIO m) => Path.Path Path.Rel t -> m (Path.Path Path.Abs t)
getFileInConfigDir file = flip (</>) file <$> getConfigDir

-- | Get the configuration directory
getConfigDir :: (MonadIO m) => m (Path.Path Path.Abs Path.Dir)
getConfigDir = Path.getXdgDir Path.XdgConfig $ Just $(Path.mkRelDir "htagcli")

decodeChecks :: Text -> Either Error (NonEmpty Check.Check)
decodeChecks = decode' checksC

-- | Variant of 'Toml.decode' that returns our custom 'Error' type
decode' :: Toml.TomlCodec c -> Text -> Either Error c
decode' codec content =
  first (ErToml . Toml.prettyTomlDecodeErrors) $
    Toml.decode codec content

checksC :: Toml.TomlCodec (NonEmpty Check.Check)
checksC = Toml.nonEmpty checkC "checks"

checkC :: Toml.TomlCodec Check.Check
checkC =
  Toml.dimatch matchTags Check.TagsExist tagsC
    <|> Toml.dimatch matchGenre Check.GenreAmong genreAmongC
    <|> Toml.dimatch
      matchFilenameMatches
      (uncurry Check.FilenameMatches)
      filenameMatchesC

matchTags :: Check.Check -> Maybe (NonEmpty Tag.Tag)
matchTags (Check.TagsExist tags) = Just tags
matchTags _ = Nothing

matchGenre :: Check.Check -> Maybe (NonEmpty Text)
matchGenre (Check.GenreAmong genres) = Just genres
matchGenre _ = Nothing

matchFilenameMatches ::
  Check.Check -> Maybe (Pattern.Pattern, Pattern.Formatting)
matchFilenameMatches (Check.FilenameMatches pattern formatting) =
  Just (pattern, formatting)
matchFilenameMatches _ = Nothing

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

filenameMatchesC :: Toml.TomlCodec (Pattern.Pattern, Pattern.Formatting)
filenameMatchesC = Toml.pair patternC formattingC

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
    <$> slashesC "slashes" .= Pattern.foSlashes
    <*> spacesC "spaces" .= Pattern.foSpaces
    <*> Toml.int "pad_track_numbers" .= Pattern.foPadTrackNumbers

slashesC :: Toml.Key -> Toml.TomlCodec Pattern.Slashes
slashesC = Toml.textBy Pattern.slashesAsText Pattern.parseSlashes

spacesC :: Toml.Key -> Toml.TomlCodec Pattern.Spaces
spacesC = Toml.textBy Pattern.spacesAsText Pattern.parseSpaces
