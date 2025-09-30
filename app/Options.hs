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

import Check qualified
import Options.Applicative qualified as Options
import Options.Applicative.NonEmpty qualified as Options
import Path qualified
import Sound.HTagLib qualified as HTagLib
import Tag qualified

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

data Options = Display DisplayOptions | Edit EditOptions | Check CheckOptions
  deriving (Show)

optionsInfo :: Options.ParserInfo Options
optionsInfo = Options.info (optionsP <**> Options.helper) Options.idm

displayOptionsP :: Options.Parser DisplayOptions
displayOptionsP = DisplayOptions <$> filesOrDirectoryP

checkOptionsP :: Options.Parser CheckOptions
checkOptionsP = CheckOptions <$> filesOrDirectoryP <*> optional checksP

checksP :: Options.Parser (NonEmpty Check.Check)
checksP =
  Options.some1
    (Check.TagsExist <$> tagsP <|> Check.GenreAmong <$> genreAmongP)

tagsP :: Options.Parser (NonEmpty Tag.Tag)
tagsP =
  Options.some1
    ( Options.option
        (Options.maybeReader Tag.parse)
        ( Options.long "tag"
            <> Options.metavar "TAG"
            <> Options.help
              "Specify a tag to check (title, artist, album, genre, year, track)"
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
              (Options.progDesc "Check tags")
          )
    )
