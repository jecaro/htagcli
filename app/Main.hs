module Main where

import Options
import Options.Applicative (execParser)
import Path (File, SomeBase, prjSomeBase, toFilePath)
import Sound.HTagLib
  ( Album,
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
  { atFile :: !(SomeBase File),
    atTitle :: !Title,
    atArtist :: !Artist,
    atAlbum :: !Album,
    atGenre :: !Genre,
    atYear :: !(Maybe Year),
    atTrack :: !(Maybe TrackNumber)
  }
  deriving (Show)

render :: AudioTrack -> Text
render AudioTrack {..} =
  unlines
    [ "File: " <> toText (prjSomeBase toFilePath atFile),
      "Title: " <> unTitle atTitle,
      "Artist: " <> unArtist atArtist,
      "Album: " <> unAlbum atAlbum,
      "Genre: " <> unGenre atGenre,
      "Year: " <> withMissing unYear atYear,
      "Track: " <> withMissing unTrackNumber atTrack
    ]

withMissing :: (Show b) => (a -> b) -> Maybe a -> Text
withMissing _ Nothing = "missing"
withMissing f (Just x) = show . f $ x

audioTrackGetter :: SomeBase File -> TagGetter AudioTrack
audioTrackGetter path =
  AudioTrack path
    <$> titleGetter
    <*> artistGetter
    <*> albumGetter
    <*> genreGetter
    <*> yearGetter
    <*> trackNumberGetter

main :: IO ()
main = do
  options <- execParser optionsInfo
  case options of
    Display DisplayOptions {..} -> do
      let filename = prjSomeBase toFilePath doFile
      track <- getTags filename $ audioTrackGetter doFile
      putTextLn $ render track
    Edit EditOptions {..} -> do
      let filename = prjSomeBase toFilePath eoFile
          setter =
            fold $
              catMaybes
                [ titleSetter <$> eoTitle,
                  artistSetter <$> eoArtist,
                  albumSetter <$> eoAlbum,
                  genreSetter <$> eoGenre,
                  toSetter yearSetter eoYear,
                  toSetter trackNumberSetter eoTrack
                ]
      setTags filename Nothing setter
  where
    toSetter _ Nothing = Nothing
    toSetter setter (Just Remove) = Just $ setter Nothing
    toSetter setter (Just (Set v)) = Just . setter $ Just v
