module Options
  ( CheckOptions (..),
    Directory (..),
    DisplayOptions (..),
    EditOptions (..),
    Files (..),
    FilesOrDirectory (..),
    Options (..),
    SetOrRemove (..),
    optionsInfo,
  )
where

import Check (Check (..))
import Options.Applicative
  ( Parser,
    ParserInfo,
    argument,
    command,
    help,
    helper,
    hsubparser,
    idm,
    info,
    long,
    maybeReader,
    metavar,
    option,
    progDesc,
    strOption,
  )
import Options.Applicative.Builder (flag')
import Options.Applicative.NonEmpty (some1)
import Path
  ( Dir,
    File,
    SomeBase,
    parseSomeDir,
    parseSomeFile,
  )
import Sound.HTagLib
  ( Album,
    Artist,
    Genre,
    Title,
    TrackNumber,
    Year,
    mkTrackNumber,
    mkYear,
  )
import Tag (Tag, parse)

newtype DisplayOptions = DisplayOptions
  { doFilesOrDirectory :: FilesOrDirectory
  }
  deriving (Show)

data SetOrRemove a = Set a | Remove
  deriving (Show)

data Directory = Directory
  { diPath :: SomeBase Dir,
    diExtensions :: [Text]
  }
  deriving (Show)

newtype Files = Files
  { fiFiles :: [SomeBase File]
  }
  deriving (Show)

data FilesOrDirectory
  = FDFiles Files
  | FDDirectory Directory
  deriving (Show)

data EditOptions = EditOptions
  { eoFilesOrDirectory :: FilesOrDirectory,
    eoTitle :: Maybe Title,
    eoArtist :: Maybe Artist,
    eoAlbum :: Maybe Album,
    eoGenre :: Maybe Genre,
    eoYear :: Maybe (SetOrRemove Year),
    eoTrack :: Maybe (SetOrRemove TrackNumber)
  }
  deriving (Show)

data CheckOptions = CheckOptions
  { coFilesOrDirectory :: FilesOrDirectory,
    coChecks :: NonEmpty Check
  }
  deriving (Show)

data Options = Display DisplayOptions | Edit EditOptions | Check CheckOptions
  deriving (Show)

optionsInfo :: ParserInfo Options
optionsInfo = info (optionsP <**> helper) idm

displayOptionsP :: Parser DisplayOptions
displayOptionsP = DisplayOptions <$> filesOrDirectoryP

checkOptionsP :: Parser CheckOptions
checkOptionsP = CheckOptions <$> filesOrDirectoryP <*> checksP

checksP :: Parser (NonEmpty Check)
checksP = some1 (TagsExist <$> tagsP <|> GenreAmong <$> genreAmongP)

tagsP :: Parser (NonEmpty Tag)
tagsP =
  some1
    ( option
        (maybeReader parse)
        ( long "tag"
            <> metavar "TAG"
            <> help
              "Specify a tag to check (title, artist, album, genre, year, track)"
        )
    )

genreAmongP :: Parser (NonEmpty Text)
genreAmongP =
  some1
    ( strOption
        ( long "genre-among"
            <> metavar "GENRE"
            <> help "Specify a genre to check against"
        )
    )

editOptionsP :: Parser EditOptions
editOptionsP =
  EditOptions
    <$> filesOrDirectoryP
    <*> optional
      ( strOption
          ( long "title"
              <> metavar "TITLE"
              <> help "Set the title"
          )
      )
    <*> optional
      ( strOption
          ( long "artist"
              <> metavar "ARTIST"
              <> help "Set the artist"
          )
      )
    <*> optional
      ( strOption
          ( long "album"
              <> metavar "ALBUM"
              <> help "Set the album"
          )
      )
    <*> optional
      ( strOption
          ( long "genre"
              <> metavar "GENRE"
              <> help "Set the genre"
          )
      )
    <*> optional
      ( option
          (maybeReader strToYear)
          ( long "year"
              <> metavar "YEAR"
              <> help "Set the year"
          )
          <|> flag'
            Remove
            ( long "noyear"
                <> help "Unset the year"
            )
      )
    <*> optional
      ( option
          (maybeReader strToTrackNumber)
          ( long "track"
              <> metavar "TRACK"
              <> help "Set the track number"
          )
          <|> flag'
            Remove
            ( long "notrack"
                <> help "Unset the track"
            )
      )
  where
    strToYear :: String -> Maybe (SetOrRemove Year)
    strToYear = fmap Set . mkYear <=< readMaybe
    strToTrackNumber :: String -> Maybe (SetOrRemove TrackNumber)
    strToTrackNumber = fmap Set . mkTrackNumber <=< readMaybe

filesOrDirectoryP :: Parser FilesOrDirectory
filesOrDirectoryP =
  hsubparser
    ( command
        "files"
        (info (FDFiles . Files <$> someBaseFilesP) (progDesc "Process files"))
        <> command
          "directory"
          ( info
              (mkDirectory <$> someBaseDirP <*> extensionsP)
              ( progDesc
                  "Recursively process files in a directory with specified extensions"
              )
          )
    )
  where
    mkDirectory directory extensions =
      FDDirectory $ Directory directory extensions

someBaseFilesP :: Parser [SomeBase File]
someBaseFilesP = some $ argument (maybeReader parseSomeFile) (metavar "FILES")

extensionsP :: Parser [Text]
extensionsP =
  some (fromString <$> strOption (long "extension" <> metavar "EXTENSION"))

someBaseDirP :: Parser (SomeBase Dir)
someBaseDirP = argument (maybeReader parseSomeDir) (metavar "DIRECTORY")

optionsP :: Parser Options
optionsP =
  hsubparser
    ( command
        "display"
        (info (Display <$> displayOptionsP) (progDesc "Show tags"))
        <> command
          "edit"
          (info (Edit <$> editOptionsP) (progDesc "Edit tags"))
        <> command
          "check"
          (info (Check <$> checkOptionsP) (progDesc "Check tags"))
    )
