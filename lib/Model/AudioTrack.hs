module Model.AudioTrack
  ( AudioTrack (..),
    albumArtistOrArtist,
    haveTag,
    getTags,
    setTags,
    asText,
    parser,
    audioTracksP,
  )
where

import Data.Set qualified as Set
import Data.Text qualified as Text
import Path qualified
import Model.SetTagsOptions qualified as SetTagsOptions
import Sound.HTagLib qualified as HTagLib
import Sound.HTagLib.Extra qualified as HTagLib
import Model.Tag qualified as Tag
import Text.Megaparsec qualified as Megaparsec
import Text.Megaparsec.Char qualified as Megaparsec
import Text.Megaparsec.Char.Lexer qualified as MegaparsecL

type Parser = Megaparsec.Parsec Void Text

data AudioTrack = AudioTrack
  { atFile :: Path.Path Path.Abs Path.File,
    atTitle :: HTagLib.Title,
    atArtist :: HTagLib.Artist,
    atAlbumArtist :: HTagLib.AlbumArtist,
    atAlbum :: HTagLib.Album,
    atDisc :: Maybe HTagLib.DiscNumber,
    atGenre :: HTagLib.Genre,
    atYear :: Maybe HTagLib.Year,
    atTrack :: Maybe HTagLib.TrackNumber
  }
  deriving (Eq, Show)

albumArtistOrArtist :: AudioTrack -> Text
albumArtistOrArtist track
  | haveTag Tag.AlbumArtist track = HTagLib.unAlbumArtist (atAlbumArtist track)
  | otherwise = HTagLib.unArtist (atArtist track)

parser :: Parser AudioTrack
parser = do
  fileText <- lineP "File:" notEol id
  atFile <- case Path.parseAbsFile (toString fileText) of
    Left err -> fail $ "Failed to parse file path: " <> show err
    Right path -> pure path

  atTitle <- lineP "Title:" notEol HTagLib.mkTitle
  atArtist <- lineP "Artist:" notEol HTagLib.mkArtist
  atAlbumArtist <- lineP "Album Artist:" notEol HTagLib.mkAlbumArtist
  atAlbum <- lineP "Album:" notEol HTagLib.mkAlbum
  atDisc <-
    lineP
      "Disc:"
      (Megaparsec.optional MegaparsecL.decimal)
      (HTagLib.mkDiscNumber =<<)
  atGenre <- lineP "Genre:" notEol HTagLib.mkGenre
  atYear <-
    lineP "Year:" (Megaparsec.optional MegaparsecL.decimal) (HTagLib.mkYear =<<)
  atTrack <-
    lineP
      "Track:"
      (Megaparsec.optional MegaparsecL.decimal)
      (HTagLib.mkTrackNumber =<<)

  pure AudioTrack {..}

audioTracksP :: Parser [AudioTrack]
audioTracksP =
  Megaparsec.sepEndBy
    parser
    (Megaparsec.many Megaparsec.newline)
    <* Megaparsec.eof

notEol :: Parser Text
notEol = Megaparsec.takeWhileP Nothing (/= '\n')

lineP :: Text -> Parser b -> (b -> a) -> Parser a
lineP prefix inner constructor = do
  void $ lexeme $ Megaparsec.string prefix
  value <- lexeme inner
  void Megaparsec.newline
  pure $ constructor value

spaceConsumer :: Parser ()
spaceConsumer = MegaparsecL.space Megaparsec.hspace1 simpleFailure simpleFailure
  where
    simpleFailure = Megaparsec.failure Nothing Set.empty

lexeme :: Parser a -> Parser a
lexeme = MegaparsecL.lexeme spaceConsumer

asText :: AudioTrack -> Text
asText AudioTrack {..} =
  unlines
    [ "File: " <> toText (Path.toFilePath atFile),
      "Title: " <> HTagLib.unTitle atTitle,
      "Artist: " <> HTagLib.unArtist atArtist,
      "Album Artist: " <> HTagLib.unAlbumArtist atAlbumArtist,
      "Album: " <> HTagLib.unAlbum atAlbum,
      "Disc: " <> withMissing HTagLib.unDiscNumber atDisc,
      "Genre: " <> HTagLib.unGenre atGenre,
      "Year: " <> withMissing HTagLib.unYear atYear,
      "Track: " <> withMissing HTagLib.unTrackNumber atTrack
    ]

withMissing :: (Show b) => (a -> b) -> Maybe a -> Text
withMissing _ Nothing = ""
withMissing f (Just x) = show . f $ x

getTags :: (MonadIO m) => Path.Path Path.Abs Path.File -> m AudioTrack
getTags file = do
  let fileStr = Path.toFilePath file
  HTagLib.getTags fileStr $ getter file

getter :: Path.Path Path.Abs Path.File -> HTagLib.TagGetter AudioTrack
getter path =
  AudioTrack path
    <$> HTagLib.titleGetter
    <*> HTagLib.artistGetter
    <*> HTagLib.albumArtistGetter
    <*> HTagLib.albumGetter
    <*> HTagLib.discNumberGetter
    <*> HTagLib.genreGetter
    <*> HTagLib.yearGetter
    <*> HTagLib.trackNumberGetter

setTags :: (MonadIO m) => AudioTrack -> m ()
setTags track@AudioTrack {..} =
  HTagLib.setTags (Path.toFilePath atFile) Nothing $ setter track

setter :: AudioTrack -> HTagLib.TagSetter
setter AudioTrack {..} =
  SetTagsOptions.setter $
    SetTagsOptions.SetTagsOptions
      { seTitle = Just atTitle,
        seArtist = Just atArtist,
        seAlbum = Just atAlbum,
        seAlbumArtist = Just atAlbumArtist,
        seGenre = Just atGenre,
        seYear = setOrRemove atYear,
        seTrack = setOrRemove atTrack,
        seDisc = setOrRemove atDisc
      }
  where
    setOrRemove = Just . maybe SetTagsOptions.Remove SetTagsOptions.Set

haveTag :: Tag.Tag -> AudioTrack -> Bool
haveTag Tag.Title = not . Text.null . HTagLib.unTitle . atTitle
haveTag Tag.Artist = not . Text.null . HTagLib.unArtist . atArtist
haveTag Tag.Album = not . Text.null . HTagLib.unAlbum . atAlbum
haveTag Tag.AlbumArtist = not . Text.null . HTagLib.unAlbumArtist . atAlbumArtist
haveTag Tag.Genre = not . Text.null . HTagLib.unGenre . atGenre
haveTag Tag.Year = isJust . atYear
haveTag Tag.Track = isJust . atTrack
haveTag Tag.Disc = isJust . atDisc
