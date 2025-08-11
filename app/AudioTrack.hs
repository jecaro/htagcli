module AudioTrack (AudioTrack (..), getTags, render) where

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
    artistGetter,
    genreGetter,
    titleGetter,
    trackNumberGetter,
    unAlbum,
    unArtist,
    unGenre,
    unTitle,
    unTrackNumber,
    unYear,
    yearGetter,
  )
import qualified Sound.HTagLib as HTagLib

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

getTags :: SomeBase File -> IO AudioTrack
getTags file = do
  let fileStr = prjSomeBase toFilePath file
  HTagLib.getTags fileStr $ getter file

getter :: SomeBase File -> TagGetter AudioTrack
getter path =
  AudioTrack path
    <$> titleGetter
    <*> artistGetter
    <*> albumGetter
    <*> genreGetter
    <*> yearGetter
    <*> trackNumberGetter
