module Options
  ( CheckOptions (..),
    Command (..),
    Directory (..),
    Files (..),
    FilesOrDirectory (..),
    FixFilePathsOptions (..),
    optionsInfo,
  )
where

import Check.Album qualified as Album
import Check.File qualified as File
import Data.List.Extra qualified as List
import Options.Applicative qualified as Options
import Options.Applicative.NonEmpty qualified as Options
import Path qualified
import Pattern qualified
import SetTagsOptions qualified
import Sound.HTagLib qualified as HTagLib
import Sound.HTagLib.Extra qualified as HTagLib
import Tag qualified
import Text.Megaparsec qualified as Megaparsec

data Directory = Directory
  { diPath :: Path.SomeBase Path.Dir,
    diExtensions :: NonEmpty Text
  }
  deriving (Show)

newtype Files = Files
  { fiFiles :: NonEmpty (Path.SomeBase Path.File)
  }
  deriving (Show)

data FilesOrDirectory
  = FDFiles Files
  | FDDirectory Directory
  deriving (Show)

data CheckOptions = CheckOptions
  { coFileChecks :: [File.Check],
    coAlbumChecks :: [Album.Check]
  }
  deriving (Show)

data FixFilePathsOptions = FixFilePathsOptions
  { foDryRun :: Bool,
    foBaseDirectory :: Maybe (Path.SomeBase Path.Dir),
    foFormatting :: Maybe Pattern.Formatting,
    foPattern :: Maybe Pattern.Pattern
  }
  deriving (Show)

data Command
  = CreateConfig
  | GetTags FilesOrDirectory
  | SetTags SetTagsOptions.SetTagsOptions FilesOrDirectory
  | Edit FilesOrDirectory
  | Check CheckOptions FilesOrDirectory
  | FixFilePaths FixFilePathsOptions FilesOrDirectory
  deriving (Show)

optionsInfo :: Options.ParserInfo Command
optionsInfo = Options.info (optionsP <**> Options.helper) Options.idm

checkOptionsP :: Options.Parser CheckOptions
checkOptionsP = CheckOptions <$> checksP <*> albumChecksP

fixFilePathsOptionsP :: Options.Parser FixFilePathsOptions
fixFilePathsOptionsP =
  FixFilePathsOptions
    <$> dryRunP
    <*> optional baseDirectoryP
    <*> optional formattingP
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
    )

checksP :: Options.Parser [File.Check]
checksP =
  Options.many
    ( File.TagsExist
        <$> tagsP
          <|> File.GenreAmong
        <$> genreAmongP
          <|> File.FilenameMatches
        <$> filematchesP
        <*> formattingP
    )

albumChecksP :: Options.Parser [Album.Check]
albumChecksP =
  Options.many
    ( Album.InSameDir
        <$ Options.flag'
          ()
          ( Options.long "in-same-dir"
              <> Options.help "Check that all tracks are in the same directory"
          )
          <|> Album.HaveCover
        <$> Options.some1
          ( Options.option
              (Options.maybeReader Path.parseRelFile)
              ( Options.long "have-cover"
                  <> Options.metavar "FILENAME"
                  <> Options.help "Check that the specified cover file exists"
              )
          )
          <|> Album.SameTag
        <$> Options.some1
          ( Options.option
              tagR
              ( Options.long "album-tag"
                  <> Options.metavar "TAG"
                  <> Options.help
                    "Check that all tracks in the album have the same value for \
                    \the specified tag (title, artist, album, albumartist, \
                    \genre, year, track)"
              )
          )
    )

tagR :: Options.ReadM Tag.Tag
tagR = Options.maybeReader $ Megaparsec.parseMaybe Tag.parser . toText

tagsP :: Options.Parser (NonEmpty Tag.Tag)
tagsP =
  Options.some1
    ( Options.option
        tagR
        ( Options.long "tag"
            <> Options.metavar "TAG"
            <> Options.help
              "Specify a tag to check (title, artist, album, albumartist, \
              \genre, year, track)"
        )
    )

genreAmongP :: Options.Parser (NonEmpty Text)
genreAmongP =
  Options.some1
    ( Options.strOption
        ( Options.long "genre-among"
            <> Options.metavar "GENRE"
            <> Options.help "Specify a genre to check against"
        )
    )

filematchesP :: Options.Parser Pattern.Pattern
filematchesP =
  Options.option
    (Options.maybeReader $ parse . toText)
    ( Options.long "filematches"
        <> Options.metavar "PATTERN"
        <> Options.help
          "Specify a filename pattern to check against \
          \(example: {genre}/{artist}/{album}/{tracknumber} - {title})"
    )
  where
    parse = Megaparsec.parseMaybe Pattern.parser

formattingP :: Options.Parser Pattern.Formatting
formattingP =
  Pattern.Formatting
    <$> charToCharActionP
    <*> paddingP "pad track"
    <*> paddingP "pad disc"

charToCharActionP :: Options.Parser [(Char, Pattern.CharAction)]
charToCharActionP =
  Pattern.addSlashIfNeeded
    <$> Options.many
      ( (,Pattern.ChRemove)
          <$> Options.option
            Options.auto
            (Options.long "remove" <> Options.metavar "CHAR")
            <|> second Pattern.ChReplace
          <$> Options.option
            (Options.eitherReader $ first toString . parse)
            (Options.long "replace" <> Options.metavar "CHAR")
      )
  where
    parse :: String -> Either Text (Char, Char)
    parse [c1, ':', c2] = Right (c1, c2)
    parse _ = Left "Must be in the form 'x:y'"

paddingP :: String -> Options.Parser Pattern.Padding
paddingP label =
  Options.option
    (Options.eitherReader $ first toString . Pattern.parsePadding . toText)
    ( Options.long option
        <> Options.metavar "N"
        <> Options.help
          ("Number of digits to " <> label <> " numbers to (default: ignore)")
    )
  where
    option = List.replace " " "-" label

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
          ( Options.long "album"
              <> Options.metavar "ALBUM"
              <> Options.help "Set the album"
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
  where
    strToYear = strTo HTagLib.mkYear
    strToTrackNumber = strTo HTagLib.mkTrackNumber
    strToDiscNumber = strTo HTagLib.mkDiscNumber
    strTo mkData = fmap SetTagsOptions.Set . mkData <=< readMaybe

filesOrDirectoryP :: Options.Parser FilesOrDirectory
filesOrDirectoryP =
  Options.hsubparser
    ( Options.command
        "files"
        ( Options.info
            (FDFiles . Files <$> someBaseFilesP)
            (Options.progDesc "Process files")
        )
        <> Options.command
          "directory"
          ( Options.info
              (mkDirectory <$> someBaseDirP <*> extensionsP)
              ( Options.progDesc
                  "Recursively process files in a directory with specified extensions"
              )
          )
    )
  where
    mkDirectory directory extensions =
      FDDirectory $ Directory directory extensions

someBaseFilesP :: Options.Parser (NonEmpty (Path.SomeBase Path.File))
someBaseFilesP =
  Options.some1 $
    Options.argument
      (Options.maybeReader Path.parseSomeFile)
      (Options.metavar "FILES")

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

someBaseDirP :: Options.Parser (Path.SomeBase Path.Dir)
someBaseDirP =
  Options.argument
    (Options.maybeReader Path.parseSomeDir)
    (Options.metavar "DIRECTORY")

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
              (GetTags <$> filesOrDirectoryP)
              (Options.progDesc "Get tags")
          )
        <> Options.command
          "set"
          ( Options.info
              (SetTags <$> setTagsOptionsP <*> filesOrDirectoryP)
              (Options.progDesc "Set tags")
          )
        <> Options.command
          "edit"
          ( Options.info
              (Edit <$> filesOrDirectoryP)
              (Options.progDesc "Edit tags in $EDITOR")
          )
        <> Options.command
          "check"
          ( Options.info
              (Check <$> checkOptionsP <*> filesOrDirectoryP)
              (Options.progDesc "Check various properties of files")
          )
        <> Options.command
          "fix-paths"
          ( Options.info
              (FixFilePaths <$> fixFilePathsOptionsP <*> filesOrDirectoryP)
              (Options.progDesc "Fix file paths according to a pattern")
          )
    )
