module SetTagsOptions
  ( SetTagsOptions (..),
    SetOrRemove (..),
    noSetTagsOptions,
    setter,
  )
where

import Sound.HTagLib qualified as HTagLib
import Sound.HTagLib.Extra qualified as HTagLib

data SetOrRemove a = Set a | Remove
  deriving (Show)

data SetTagsOptions = SetTagsOptions
  { seTitle :: Maybe HTagLib.Title,
    seArtist :: Maybe HTagLib.Artist,
    seAlbum :: Maybe HTagLib.Album,
    seAlbumArtist :: Maybe HTagLib.AlbumArtist,
    seGenre :: Maybe HTagLib.Genre,
    seYear :: Maybe (SetOrRemove HTagLib.Year),
    seTrack :: Maybe (SetOrRemove HTagLib.TrackNumber)
  }
  deriving (Show)

noSetTagsOptions :: SetTagsOptions
noSetTagsOptions =
  SetTagsOptions
    { seTitle = Nothing,
      seArtist = Nothing,
      seAlbum = Nothing,
      seAlbumArtist = Nothing,
      seGenre = Nothing,
      seYear = Nothing,
      seTrack = Nothing
    }

setter :: SetTagsOptions -> HTagLib.TagSetter
setter SetTagsOptions {..} =
  fold $
    catMaybes
      [ HTagLib.titleSetter <$> seTitle,
        HTagLib.artistSetter <$> seArtist,
        HTagLib.albumSetter <$> seAlbum,
        HTagLib.albumArtistSetter <$> seAlbumArtist,
        HTagLib.genreSetter <$> seGenre,
        toSetter HTagLib.yearSetter seYear,
        toSetter HTagLib.trackNumberSetter seTrack
      ]
  where
    toSetter _ Nothing = Nothing
    toSetter tagSetter (Just SetTagsOptions.Remove) = Just $ tagSetter Nothing
    toSetter tagSetter (Just (SetTagsOptions.Set v)) = Just . tagSetter $ Just v
