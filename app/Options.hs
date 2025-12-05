module Options
  ( CheckOptions (..),
    Command (..),
    Files (..),
    FixFilePathsOptions (..),
    optionsInfo,
    checks,
  )
where

import Check.Album qualified as Album
import Check.Artist qualified as Artist
import Check.Track qualified as Track
import Config qualified
import Data.Text qualified as Text
import Model.Cover qualified as Cover
import Model.Pattern qualified as Pattern
import Model.SetTagsOptions qualified as SetTagsOptions
import Model.Tag qualified as Tag
import Options.Applicative qualified as Options
import Options.Applicative.NonEmpty qualified as Options
import Path qualified
import Sound.HTagLib qualified as HTagLib
import Sound.HTagLib.Extra qualified as HTagLib
import Text.Megaparsec qualified as Megaparsec

data Files = Files
  { fiPaths :: NonEmpty Text,
    fiExtensions :: NonEmpty Text
  }
  deriving (Show)

data CheckOptions = CheckOptions
  { chChecks :: Config.Checks,
    chFilematches :: Maybe Pattern.Pattern
  }
  deriving (Show)

data FixFilePathsOptions = FixFilePathsOptions
  { foDryRun :: Bool,
    foBaseDirectory :: Maybe (Path.SomeBase Path.Dir),
    foPattern :: Maybe Pattern.Pattern
  }
  deriving (Show)

data Command
  = CreateConfig
  | GetTags Files
  | SetTags SetTagsOptions.SetTagsOptions Files
  | Edit Files
  | Check CheckOptions Files
  | FixFilePaths FixFilePathsOptions Files
  deriving (Show)

-- | Get checks from the CLI, and fall back to the config file if none are
-- specified
checks ::
  Config.Config ->
  CheckOptions ->
  ([Track.Check], [Album.Check], Maybe Artist.Check)
checks
  config@(Config.Config {coFilename = Config.Filename {..}})
  (Options.CheckOptions {..})
    | not $ Config.haveChecks chChecks = Config.factorChecks config
    | otherwise = Config.factorChecks' pattern fiFormatting chChecks
    where
      pattern = fromMaybe fiPattern chFilematches

optionsInfo :: Options.ParserInfo Command
optionsInfo = Options.info (optionsP <**> Options.helper) Options.idm

checkOptionsP :: Options.Parser CheckOptions
checkOptionsP =
  CheckOptions
    <$> checksP
    <*> Options.optional filematchesP

checksP :: Options.Parser Config.Checks
checksP =
  Config.Checks
    <$> optional trackTagsP
    <*> optional trackGenreAmongP
    <*> trackFilenameP
    <*> optional albumHaveCoverP
    <*> albumSameDirP
    <*> optional albumSameTagsP
    <*> albumTracksSequentialP
    <*> artistSameGenreP

fixFilePathsOptionsP :: Options.Parser FixFilePathsOptions
fixFilePathsOptionsP =
  FixFilePathsOptions
    <$> dryRunP
    <*> optional baseDirectoryP
    <*> optional filematchesP

dryRunP :: Options.Parser Bool
dryRunP =
  Options.switch
    ( Options.long "dry-run"
        <> Options.help
          "Show what would be done, but don't actually rename files"
    )

baseDirectoryP :: Options.Parser (Path.SomeBase Path.Dir)
baseDirectoryP =
  Options.option
    (Options.maybeReader Path.parseSomeDir)
    ( Options.long "base-dir"
        <> Options.metavar "DIRECTORY"
        <> Options.help
          "Base directory to use with the pattern to move files"
        <> Options.action "directory"
    )

trackFilenameP :: Options.Parser Bool
trackFilenameP =
  Options.switch
    ( Options.long "track-filename"
        <> Options.help
          "Check that filenames match the specified pattern"
    )

albumHaveCoverP :: Options.Parser Cover.Cover
albumHaveCoverP =
  Cover.Cover
    <$> coverPathsP
    <*> Options.optional (coverSizeP "album-cover-min-size")
    <*> Options.optional (coverSizeP "album-cover-max-size")

coverPathsP :: Options.Parser (NonEmpty (Path.Path Path.Rel Path.File))
coverPathsP =
  Options.some1
    ( Options.option
        (Options.maybeReader Path.parseRelFile)
        ( Options.long "album-cover-filename"
            <> Options.metavar "FILENAME"
            <> Options.help "Check that the specified cover file exists"
        )
    )

coverSizeP :: String -> Options.Parser Cover.Size
coverSizeP option =
  Options.option
    (Options.eitherReader $ first toString . parse . toText)
    ( Options.long option
        <> Options.metavar "WIDTHxHEIGHT"
        <> Options.help
          ("Specify the " <> option <> " in the form WIDTHxHEIGHT")
    )
  where
    parse :: Text -> Either Text Cover.Size
    parse text =
      case Text.splitOn "x" text of
        [widthTxt, heightTxt] ->
          case (readMaybe (toString widthTxt), readMaybe (toString heightTxt)) of
            (Just siWidth, Just siHeight) -> Right $ Cover.Size {..}
            _ -> Left "Width and height must be integers"
        _ -> Left "Size must be in the form WIDTHxHEIGHT"

albumSameDirP :: Options.Parser Bool
albumSameDirP =
  Options.switch
    ( Options.long "album-same-dir"
        <> Options.help "Check that all tracks are in the same directory"
    )

albumSameTagsP :: Options.Parser (NonEmpty Tag.Tag)
albumSameTagsP =
  Options.some1
    ( Options.option
        tagR
        ( Options.long "album-same-tag"
            <> Options.metavar "TAG"
            <> Options.help
              "Check that all tracks in the album have the same value for \
              \the specified tag (title, artist, album, albumartist, genre, \
              \year, track)"
        )
    )

albumTracksSequentialP :: Options.Parser Bool
albumTracksSequentialP =
  Options.switch
    ( Options.long "album-tracks-sequential"
        <> Options.help
          "Check that track numbers are sequential within the album"
    )

artistSameGenreP :: Options.Parser (Maybe Artist.Check)
artistSameGenreP =
  Options.flag
    Nothing
    (Just $ Artist.SameGenre mempty)
    ( Options.long "artist-same-genre"
        <> Options.help
          "Check that all tracks by the same artist have the same genre"
    )

tagR :: Options.ReadM Tag.Tag
tagR = Options.maybeReader $ Megaparsec.parseMaybe Tag.parser . toText

trackTagsP :: Options.Parser (NonEmpty Tag.Tag)
trackTagsP =
  Options.some1
    ( Options.option
        tagR
        ( Options.long "track-tag"
            <> Options.metavar "TAG"
            <> Options.help
              "Specify a tag to check (title, artist, album, albumartist, \
              \genre, year, track)"
        )
    )

trackGenreAmongP :: Options.Parser (NonEmpty HTagLib.Genre)
trackGenreAmongP =
  Options.some1
    ( HTagLib.mkGenre
        <$> Options.strOption
          ( Options.long "track-genre"
              <> Options.metavar "GENRE"
              <> Options.help "Specify a genre to check against"
          )
    )

filematchesP :: Options.Parser Pattern.Pattern
filematchesP =
  Options.option
    (Options.maybeReader $ parse . toText)
    ( Options.long "pattern"
        <> Options.metavar "PATTERN"
        <> Options.help
          "Specify a filename pattern to check against. See the default config \
          \file for details."
    )
  where
    parse = Megaparsec.parseMaybe Pattern.parser

setTagsOptionsP :: Options.Parser SetTagsOptions.SetTagsOptions
setTagsOptionsP =
  SetTagsOptions.SetTagsOptions
    <$> optional
      ( Options.strOption
          ( Options.long "title"
              <> Options.metavar "TITLE"
              <> Options.help "Set the title"
          )
      )
    <*> optional
      ( Options.strOption
          ( Options.long "artist"
              <> Options.metavar "ARTIST"
              <> Options.help "Set the artist"
          )
      )
    <*> optional
      ( Options.strOption
          ( Options.long "albumartist"
              <> Options.metavar "ALBUMARTIST"
              <> Options.help "Set the album artist"
          )
      )
    <*> optional
      ( Options.strOption
          ( Options.long "album"
              <> Options.metavar "ALBUM"
              <> Options.help "Set the album"
          )
      )
    <*> optional
      ( Options.option
          (Options.maybeReader strToDiscNumber)
          ( Options.long "disc"
              <> Options.metavar "DISC"
              <> Options.help "Set the disc number"
          )
          <|> Options.flag'
            SetTagsOptions.Remove
            (Options.long "nodisc" <> Options.help "Unset the disc")
      )
    <*> optional
      ( Options.strOption
          ( Options.long "genre"
              <> Options.metavar "GENRE"
              <> Options.help "Set the genre"
          )
      )
    <*> optional
      ( Options.option
          (Options.maybeReader strToYear)
          ( Options.long "year"
              <> Options.metavar "YEAR"
              <> Options.help "Set the year"
          )
          <|> Options.flag'
            SetTagsOptions.Remove
            ( Options.long "noyear"
                <> Options.help "Unset the year"
            )
      )
    <*> optional
      ( Options.option
          (Options.maybeReader strToTrackNumber)
          ( Options.long "track"
              <> Options.metavar "TRACK"
              <> Options.help "Set the track number"
          )
          <|> Options.flag'
            SetTagsOptions.Remove
            (Options.long "notrack" <> Options.help "Unset the track")
      )
  where
    strToYear = strTo HTagLib.mkYear
    strToTrackNumber = strTo HTagLib.mkTrackNumber
    strToDiscNumber = strTo HTagLib.mkDiscNumber
    strTo mkData = fmap SetTagsOptions.Set . mkData <=< readMaybe

filesP :: Options.Parser Files
filesP =
  Files
    <$> filesOrDirectoriesP
    <*> extensionsP

filesOrDirectoriesP :: Options.Parser (NonEmpty Text)
filesOrDirectoriesP =
  Options.some1
    ( fromString
        <$> Options.argument
          Options.str
          (Options.metavar "FILE|DIRECTORY" <> Options.action "file")
    )

extensionsP :: Options.Parser (NonEmpty Text)
extensionsP =
  Options.some1
    ( fromString
        <$> Options.strOption
          ( Options.long "extension"
              <> Options.metavar "EXTENSION"
          )
    )
    -- Default to a sensitive set of common audio file extensions
    <|> pure (fromList ["m4a", "mp3", "flac", "ogg", "wma"])

optionsP :: Options.Parser Command
optionsP =
  Options.hsubparser
    ( Options.command
        "create-config"
        ( Options.info
            (pure CreateConfig)
            (Options.progDesc "Create a default configuration file")
        )
        <> Options.command
          "get"
          ( Options.info
              (GetTags <$> filesP)
              (Options.progDesc "Get tags")
          )
        <> Options.command
          "set"
          ( Options.info
              (SetTags <$> setTagsOptionsP <*> filesP)
              (Options.progDesc "Set tags")
          )
        <> Options.command
          "edit"
          ( Options.info
              (Edit <$> filesP)
              (Options.progDesc "Edit tags in $EDITOR")
          )
        <> Options.command
          "check"
          ( Options.info
              (Check <$> checkOptionsP <*> filesP)
              (Options.progDesc "Check various properties of files")
          )
        <> Options.command
          "fix-paths"
          ( Options.info
              (FixFilePaths <$> fixFilePathsOptionsP <*> filesP)
              (Options.progDesc "Fix file paths according to a pattern")
          )
    )
