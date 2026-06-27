module Model.SetTags
  ( SetTags (..),
    SetOrRemove (..),
    noSetTags,
    setter,
  )
where

import Sound.HTagLib qualified as HTagLib
import Sound.HTagLib.Extra qualified as HTagLib

data SetOrRemove a = Set a | Remove
  deriving (Show)

data SetTags = SetTags
  { seTitle :: Maybe HTagLib.Title,
    seArtist :: Maybe HTagLib.Artist,
    seAlbumArtist :: Maybe HTagLib.AlbumArtist,
    seAlbum :: Maybe HTagLib.Album,
    seDisc :: Maybe (SetOrRemove HTagLib.DiscNumber),
    seGenre :: Maybe HTagLib.Genre,
    seYear :: Maybe (SetOrRemove HTagLib.Year),
    seTrack :: Maybe (SetOrRemove HTagLib.TrackNumber)
  }
  deriving (Show)

noSetTags :: SetTags
noSetTags =
  SetTags
    { seTitle = Nothing,
      seArtist = Nothing,
      seAlbum = Nothing,
      seAlbumArtist = Nothing,
      seGenre = Nothing,
      seYear = Nothing,
      seTrack = Nothing,
      seDisc = Nothing
    }

setter :: SetTags -> HTagLib.TagSetter
setter SetTags {..} =
  fold $
    catMaybes
      [ HTagLib.titleSetter <$> seTitle,
        HTagLib.artistSetter <$> seArtist,
        HTagLib.albumSetter <$> seAlbum,
        HTagLib.albumArtistSetter <$> seAlbumArtist,
        toSetter HTagLib.discNumberSetter seDisc,
        HTagLib.genreSetter <$> seGenre,
        toSetter HTagLib.yearSetter seYear,
        toSetter HTagLib.trackNumberSetter seTrack
      ]
  where
    toSetter _ Nothing = Nothing
    toSetter tagSetter (Just Remove) = Just $ tagSetter Nothing
    toSetter tagSetter (Just (Set v)) = Just $ tagSetter $ Just v
