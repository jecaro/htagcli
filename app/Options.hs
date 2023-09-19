module Options (
    CheckOptions (..),
    DisplayOptions (..),
    EditOptions (..),
    Options (..),
    SetOrRemove (..),
    optionsInfo,
) where

import Options.Applicative (
    Parser,
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
import Path (
    Dir,
    File,
    SomeBase,
    parseSomeDir,
    parseSomeFile,
 )
import Sound.HTagLib (
    Album,
    Artist,
    Genre,
    Title,
    TrackNumber,
    Year,
    mkTrackNumber,
    mkYear,
 )

newtype DisplayOptions = DisplayOptions
    {doFile :: SomeBase File}
    deriving (Show)

data SetOrRemove a = Set a | Remove
    deriving (Show)

data EditOptions = EditOptions
    { eoFile :: !(SomeBase File)
    , eoTitle :: !(Maybe Title)
    , eoArtist :: !(Maybe Artist)
    , eoAlbum :: !(Maybe Album)
    , eoGenre :: !(Maybe Genre)
    , eoYear :: !(Maybe (SetOrRemove Year))
    , eoTrack :: !(Maybe (SetOrRemove TrackNumber))
    }
    deriving (Show)

newtype CheckOptions = CheckOptions
    {coDirectory :: SomeBase Dir}
    deriving (Show)

data Options = Display DisplayOptions | Edit EditOptions | Check CheckOptions
    deriving (Show)

optionsInfo :: ParserInfo Options
optionsInfo = info (optionsP <**> helper) idm

checkOptionsP :: Parser CheckOptions
checkOptionsP =
    CheckOptions
        <$> argument
            (maybeReader parseSomeDir)
            (metavar "FILE")

displayOptionsP :: Parser DisplayOptions
displayOptionsP =
    DisplayOptions
        <$> argument
            (maybeReader parseSomeFile)
            (metavar "FILE")

editOptionsP :: Parser EditOptions
editOptionsP =
    EditOptions
        <$> argument
            (maybeReader parseSomeFile)
            (metavar "FILE")
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
                (info (Check <$> checkOptionsP) (progDesc "Check album"))
        )
