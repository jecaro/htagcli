{-# LANGUAGE TemplateHaskell #-}

module Main where

import Config (Config (..), defaultConfig)
import Data.Aeson (encode)
import Data.Yaml (ToJSON (toJSON), decodeFileThrow, encodeFile)
import Options (
    CheckOptions (..),
    DisplayOptions (..),
    EditOptions (..),
    Options (..),
    SetOrRemove (..),
    optionsInfo,
 )
import Options.Applicative (execParser)
import Path (Abs, Dir, Path, mkRelDir, mkRelFile, prjSomeBase, toFilePath, (</>))
import Path.IO (XdgDirectory (..), doesFileExist, ensureDir, getXdgDir)
import Sound.HTagLib (
    Album,
    Artist,
    Genre,
    TagGetter,
    Title,
    TrackNumber,
    Year,
    albumGetter,
    albumSetter,
    artistGetter,
    artistSetter,
    genreGetter,
    genreSetter,
    getTags,
    setTags,
    titleGetter,
    titleSetter,
    trackNumberGetter,
    trackNumberSetter,
    unAlbum,
    unArtist,
    unGenre,
    unTitle,
    unTrackNumber,
    unYear,
    yearGetter,
    yearSetter,
 )

data AudioTrack = AudioTrack
    { atTitle :: !Title
    , atArtist :: !Artist
    , atAlbum :: !Album
    , atGenre :: !Genre
    , atYear :: !(Maybe Year)
    , atTrack :: !(Maybe TrackNumber)
    }
    deriving (Show)

render :: AudioTrack -> Text
render AudioTrack{..} =
    unlines
        [ "Title: " <> unTitle atTitle
        , "Artist: " <> unArtist atArtist
        , "Album: " <> unAlbum atAlbum
        , "Genre: " <> unGenre atGenre
        , "Year: " <> withMissing unYear atYear
        , "Track: " <> withMissing unTrackNumber atTrack
        ]

withMissing :: Show b => (a -> b) -> Maybe a -> Text
withMissing _ Nothing = "missing"
withMissing f (Just x) = show . f $ x

audioTrackGetter :: TagGetter AudioTrack
audioTrackGetter =
    AudioTrack
        <$> titleGetter
        <*> artistGetter
        <*> albumGetter
        <*> genreGetter
        <*> yearGetter
        <*> trackNumberGetter

getConfigDir :: IO (Path Abs Dir)
getConfigDir = getXdgDir XdgConfig $ Just $(mkRelDir "hs-tag")

main :: IO ()
main = do
    options <- execParser optionsInfo
    -- let config = defaultConfig
    -- print $ encode $ toJSON config
    -- Check exception if not found create a default
    -- file not found
    -- unable to parse
    -- config :: Either ParseException Config <- decodeFileEither "/home/jc/.config/hs-tag/config.yaml"
    -- test if file is present if not create it
    configDir <- getConfigDir
    let configFile = configDir </> $(mkRelFile "config.yaml")
        configFileStr = toFilePath configFile
    ensureDir configDir
    unlessM (doesFileExist configFile) $ encodeFile configFileStr defaultConfig
    config :: Config <- decodeFileThrow $ toFilePath configFile
    print config
    case options of
        Display DisplayOptions{..} -> do
            let filename = prjSomeBase toFilePath doFile
            track <- getTags filename audioTrackGetter
            putTextLn $ render track
        Edit EditOptions{..} -> do
            let filename = prjSomeBase toFilePath eoFile
                setter =
                    fold $
                        catMaybes
                            [ titleSetter <$> eoTitle
                            , artistSetter <$> eoArtist
                            , albumSetter <$> eoAlbum
                            , genreSetter <$> eoGenre
                            , toSetter yearSetter eoYear
                            , toSetter trackNumberSetter eoTrack
                            ]
            setTags filename Nothing setter
        Check CheckOptions{..} -> do
            print "In check"
            -- test if the target is a file or a directory
            -- if file
            let dirname = prjSomeBase toFilePath coDirectory
            -- Get recursively all audio files
            -- Get tags of all files
            -- Check same artist or various artists
            -- Check same album
            -- Check same genre if same artist
            -- Check genre valid
            -- Check year/track number
            -- Check filepath
            -- Check has cover.jpg with the right size
            -- output report per directory
            return ()
  where
    toSetter _ Nothing = Nothing
    toSetter setter (Just Remove) = Just $ setter Nothing
    toSetter setter (Just (Set v)) = Just . setter $ Just v
