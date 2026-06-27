module Options
  ( CheckOptions (..),
    Command (..),
    Files (..),
    FixFilePathsOptions (..),
    SearchOptions (..),
    SearchMany (..),
    SearchManySource (..),
    SearchOne (..),
    optionsInfo,
    checks,
  )
where

import Check.Artist qualified as Artist
import Check.Disc qualified as Disc
import Config qualified
import Data.Text qualified as Text
import Data.UUID qualified as UUID
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

data SearchOptions
  = SeSearchMany SearchMany
  | SeSearchOne SearchOne
  deriving (Show)

data SearchOne = SearchOne
  { soId :: UUID.UUID,
    soFiles :: Maybe Files
  }
  deriving (Show)

data SearchMany = SearchMany
  { smMaxResults :: Int,
    smSource :: SearchManySource
  }
  deriving (Show)

data SearchManySource
  = SearchManyFromFiles Files
  | SearchManyFromArgs HTagLib.AlbumArtist HTagLib.Album
  deriving (Show)

data Command
  = CreateConfig
  | GetTags Files
  | SetTags SetTagsOptions.SetTagsOptions Files
  | Edit Files
  | Check CheckOptions Files
  | FixFilePaths FixFilePathsOptions Files
  | Search SearchOptions
  deriving (Show)

-- | Get checks from the CLI, and fall back to the config file if none are
-- specified
checks ::
  Config.Config ->
  CheckOptions ->
  Config.AllChecks
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
    <*> optional discHaveCoverP
    <*> discSameDirP
    <*> optional discSameTagsP
    <*> discTracksSequentialP
    <*> albumDiscsSequentialP
    <*> optional albumSameTagsP
    <*> artistSameGenreP

fixFilePathsOptionsP :: Options.Parser FixFilePathsOptions
fixFilePathsOptionsP =
  FixFilePathsOptions
    <$> dryRunP
    <*> optional baseDirectoryP
    <*> optional filematchesP

searchOptionsP :: Options.Parser SearchOptions
searchOptionsP = SeSearchMany <$> searchManyP <|> SeSearchOne <$> searchOneP

searchManyP :: Options.Parser SearchMany
searchManyP =
  SearchMany
    <$> Options.option
      Options.auto
      ( Options.long "max-results"
          <> Options.metavar "N"
          <> Options.value 3
          <> Options.showDefault
          <> Options.help "Maximum number of results to display"
      )
    <*> searchManySourceP

searchManySourceP :: Options.Parser SearchManySource
searchManySourceP = searchManyFromArgsP <|> searchManyFromFilesP

searchManyFromFilesP :: Options.Parser SearchManySource
searchManyFromFilesP = SearchManyFromFiles <$> filesP

searchManyFromArgsP :: Options.Parser SearchManySource
searchManyFromArgsP =
  SearchManyFromArgs
    <$> Options.strOption
      ( Options.long "artist"
          <> Options.metavar "ARTIST"
          <> Options.help "Artist name to search for"
      )
    <*> Options.strOption
      ( Options.long "album"
          <> Options.metavar "ALBUM"
          <> Options.help "Album name to search for"
      )

searchOneP :: Options.Parser SearchOne
searchOneP =
  SearchOne
    <$> Options.option
      (Options.maybeReader UUID.fromString)
      ( Options.long "id"
          <> Options.metavar "ID"
          <> Options.help "MusicBrainz release ID to search for"
      )
    <*> Options.optional filesP

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

discHaveCoverP :: Options.Parser Cover.Cover
discHaveCoverP =
  Cover.Cover
    <$> coverPathsP
    <*> Options.optional (coverSizeP "disc-cover-min-size")
    <*> Options.optional (coverSizeP "disc-cover-max-size")

coverPathsP :: Options.Parser (NonEmpty (Path.Path Path.Rel Path.File))
coverPathsP =
  Options.some1
    ( Options.option
        (Options.maybeReader Path.parseRelFile)
        ( Options.long "disc-cover-filename"
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

discSameDirP :: Options.Parser Bool
discSameDirP =
  Options.switch
    ( Options.long "disc-same-dir"
        <> Options.help "Check that all tracks are in the same directory"
    )

discSameTagsP :: Options.Parser (NonEmpty Tag.Tag)
discSameTagsP =
  Options.some1
    ( Options.option
        tagR
        ( Options.long "disc-same-tag"
            <> Options.metavar "TAG"
            <> Options.help
              "Check that all tracks in the disc have the same value for \
              \the specified tag (title, artist, album, albumartist, genre, \
              \year, track)"
        )
    )

discTracksSequentialP :: Options.Parser Bool
discTracksSequentialP =
  Options.switch
    ( Options.long "disc-tracks-sequential"
        <> Options.help
          "Check that track numbers are sequential within the disc"
    )

albumDiscsSequentialP :: Options.Parser Bool
albumDiscsSequentialP =
  Options.switch
    ( Options.long "album-discs-sequential"
        <> Options.help
          "Check that disc numbers are sequential within the album"
    )

albumSameTagsP :: Options.Parser (NonEmpty Tag.Tag)
albumSameTagsP =
  Options.some1
    ( Options.option
        tagR
        ( Options.long "album-same-tag"
            <> Options.metavar "TAG"
            <> Options.help
              "Check that all discs in the album have the same value for \
              \the specified tag (title, artist, album, albumartist, genre, \
              \year, track)"
        )
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
    ( Options.argument
        Options.str
        (Options.metavar "FILE|DIRECTORY" <> Options.action "file")
    )

extensionsP :: Options.Parser (NonEmpty Text)
extensionsP =
  Options.some1
    ( Options.strOption
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
        <> Options.command
          "search"
          ( Options.info
              (Search <$> searchOptionsP)
              (Options.progDesc "Search MusicBrainz for releases")
          )
    )
