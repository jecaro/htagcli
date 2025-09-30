module AudioTrack
  ( AudioTrack (..),
    haveTag,
    getTags,
    asText,
    format,
  )
where

import Data.Text qualified as Text
import Path qualified
import Sound.HTagLib qualified as HTagLib
import Tag qualified

data AudioTrack = AudioTrack
  { atFile :: Path.SomeBase Path.File,
    atTitle :: HTagLib.Title,
    atArtist :: HTagLib.Artist,
    atAlbum :: HTagLib.Album,
    atGenre :: HTagLib.Genre,
    atYear :: Maybe HTagLib.Year,
    atTrack :: Maybe HTagLib.TrackNumber
  }
  deriving (Show)

asText :: AudioTrack -> Text
asText AudioTrack {..} =
  unlines
    [ "File: " <> toText (Path.prjSomeBase Path.toFilePath atFile),
      "Title: " <> HTagLib.unTitle atTitle,
      "Artist: " <> HTagLib.unArtist atArtist,
      "Album: " <> HTagLib.unAlbum atAlbum,
      "Genre: " <> HTagLib.unGenre atGenre,
      "Year: " <> withMissing HTagLib.unYear atYear,
      "Track: " <> withMissing HTagLib.unTrackNumber atTrack
    ]

withMissing :: (Show b) => (a -> b) -> Maybe a -> Text
withMissing _ Nothing = "missing"
withMissing f (Just x) = show . f $ x

getTags :: (MonadIO m) => Path.SomeBase Path.File -> m AudioTrack
getTags file = do
  let fileStr = Path.prjSomeBase Path.toFilePath file
  HTagLib.getTags fileStr $ getter file

getter :: Path.SomeBase Path.File -> HTagLib.TagGetter AudioTrack
getter path =
  AudioTrack path
    <$> HTagLib.titleGetter
    <*> HTagLib.artistGetter
    <*> HTagLib.albumGetter
    <*> HTagLib.genreGetter
    <*> HTagLib.yearGetter
    <*> HTagLib.trackNumberGetter

haveTag :: Tag.Tag -> AudioTrack -> Bool
haveTag Tag.Title = not . Text.null . HTagLib.unTitle . atTitle
haveTag Tag.Artist = not . Text.null . HTagLib.unArtist . atArtist
haveTag Tag.Album = not . Text.null . HTagLib.unAlbum . atAlbum
haveTag Tag.Genre = not . Text.null . HTagLib.unGenre . atGenre
haveTag Tag.Year = isJust . atYear
haveTag Tag.Track = isJust . atTrack

format :: Tag.Tag -> AudioTrack -> Text
format Tag.Title = HTagLib.unTitle . atTitle
format Tag.Artist = HTagLib.unArtist . atArtist
format Tag.Album = HTagLib.unAlbum . atAlbum
format Tag.Genre = HTagLib.unGenre . atGenre
format Tag.Year = maybe "" (Text.pack . show . HTagLib.unYear) . atYear
format Tag.Track = maybe "" (Text.pack . show . HTagLib.unTrackNumber) . atTrack
