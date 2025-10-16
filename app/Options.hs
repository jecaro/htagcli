module Options
  ( CheckOptions (..),
    Directory (..),
    DisplayOptions (..),
    EditOptions (..),
    Files (..),
    FilesOrDirectory (..),
    Options (..),
    SetOrRemove (..),
    FixFilePathsOptions (..),
    optionsInfo,
  )
where

import Check qualified
import Options.Applicative qualified as Options
import Options.Applicative.NonEmpty qualified as Options
import Path qualified
import Pattern qualified
import Sound.HTagLib qualified as HTagLib
import Sound.HTagLib.Extra qualified as HTagLib
import Tag qualified
import Text.Megaparsec qualified as Megaparsec

newtype DisplayOptions = DisplayOptions
  { doFilesOrDirectory :: FilesOrDirectory
  }
  deriving (Show)

data SetOrRemove a = Set a | Remove
  deriving (Show)

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

data EditOptions = EditOptions
  { eoFilesOrDirectory :: FilesOrDirectory,
    eoTitle :: Maybe HTagLib.Title,
    eoArtist :: Maybe HTagLib.Artist,
    eoAlbum :: Maybe HTagLib.Album,
    eoAlbumArtist :: Maybe HTagLib.AlbumArtist,
    eoGenre :: Maybe HTagLib.Genre,
    eoYear :: Maybe (SetOrRemove HTagLib.Year),
    eoTrack :: Maybe (SetOrRemove HTagLib.TrackNumber)
  }
  deriving (Show)

data CheckOptions = CheckOptions
  { coFilesOrDirectory :: FilesOrDirectory,
    coChecks :: Maybe (NonEmpty Check.Check)
  }
  deriving (Show)

data FixFilePathsOptions = FixFilePathsOptions
  { foFilesOrDirectory :: FilesOrDirectory,
    foDryRun :: Bool,
    foBaseDirectory :: Maybe (Path.SomeBase Path.Dir),
    foFormatting :: Maybe Pattern.Formatting,
    foPattern :: Maybe Pattern.Pattern
  }
  deriving (Show)

data Options
  = Display DisplayOptions
  | Edit EditOptions
  | Check CheckOptions
  | FixFilePaths FixFilePathsOptions
  deriving (Show)

optionsInfo :: Options.ParserInfo Options
optionsInfo = Options.info (optionsP <**> Options.helper) Options.idm

displayOptionsP :: Options.Parser DisplayOptions
displayOptionsP = DisplayOptions <$> filesOrDirectoryP

checkOptionsP :: Options.Parser CheckOptions
checkOptionsP = CheckOptions <$> filesOrDirectoryP <*> optional checksP

fixFilePathsOptionsP :: Options.Parser FixFilePathsOptions
fixFilePathsOptionsP =
  FixFilePathsOptions
    <$> filesOrDirectoryP
    <*> dryRunP
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

checksP :: Options.Parser (NonEmpty Check.Check)
checksP =
  Options.some1
    ( Check.TagsExist
        <$> tagsP
          <|> Check.GenreAmong
        <$> genreAmongP
          <|> Check.FilenameMatches
        <$> filematchesP
        <*> formattingP
    )

tagsP :: Options.Parser (NonEmpty Tag.Tag)
tagsP =
  Options.some1
    ( Options.option
        (Options.maybeReader $ parse . toText)
        ( Options.long "tag"
            <> Options.metavar "TAG"
            <> Options.help
              "Specify a tag to check (title, artist, album, genre, year, track)"
        )
    )
  where
    parse = Megaparsec.parseMaybe Tag.parser

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
    <*> paddingP

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

paddingP :: Options.Parser Pattern.Padding
paddingP =
  Options.option
    (Options.eitherReader $ first toString . Pattern.parsePadding . toText)
    ( Options.long "padtrack"
        <> Options.metavar "N"
        <> Options.help
          "Number of digits to pad track numbers to (default: ignore)"
    )

editOptionsP :: Options.Parser EditOptions
editOptionsP =
  EditOptions
    <$> filesOrDirectoryP
    <*> optional
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
            Remove
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
            Remove
            (Options.long "notrack" <> Options.help "Unset the track")
      )
  where
    strToYear :: String -> Maybe (SetOrRemove HTagLib.Year)
    strToYear = fmap Set . HTagLib.mkYear <=< readMaybe
    strToTrackNumber :: String -> Maybe (SetOrRemove HTagLib.TrackNumber)
    strToTrackNumber = fmap Set . HTagLib.mkTrackNumber <=< readMaybe

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
    <|> pure ("m4a" :| ["mp3", "flac", "ogg", "wma"])

someBaseDirP :: Options.Parser (Path.SomeBase Path.Dir)
someBaseDirP =
  Options.argument
    (Options.maybeReader Path.parseSomeDir)
    (Options.metavar "DIRECTORY")

optionsP :: Options.Parser Options
optionsP =
  Options.hsubparser
    ( Options.command
        "display"
        ( Options.info
            (Display <$> displayOptionsP)
            (Options.progDesc "Show tags")
        )
        <> Options.command
          "edit"
          (Options.info (Edit <$> editOptionsP) (Options.progDesc "Edit tags"))
        <> Options.command
          "check"
          ( Options.info
              (Check <$> checkOptionsP)
              (Options.progDesc "Check various properties of files")
          )
        <> Options.command
          "fix-paths"
          ( Options.info
              (FixFilePaths <$> fixFilePathsOptionsP)
              (Options.progDesc "Fix file paths according to a pattern")
          )
    )
