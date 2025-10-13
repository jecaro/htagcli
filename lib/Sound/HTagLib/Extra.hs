module Sound.HTagLib.Extra
  ( AlbumArtist,
    mkAlbumArtist,
    unAlbumArtist,
    albumArtistGetter,
    albumArtistSetter,
  )
where

import Data.Text qualified as Text
import Sound.HTagLib qualified as HTagLib

newtype AlbumArtist = AlbumArtist Text.Text
  deriving (Show, Eq)

instance IsString AlbumArtist where
  fromString = mkAlbumArtist . fromString

unAlbumArtist :: AlbumArtist -> Text.Text
unAlbumArtist (AlbumArtist albumArtist) = albumArtist

mkAlbumArtist :: Text.Text -> AlbumArtist
mkAlbumArtist = AlbumArtist . Text.map nullToSpace
  where
    nullToSpace '\0' = ' '
    nullToSpace c = c

albumArtistKey :: Text.Text
albumArtistKey = "ALBUMARTIST"

albumArtistGetter :: HTagLib.TagGetter AlbumArtist
albumArtistGetter =
  mkAlbumArtist . headOrEmpty <$> HTagLib.propertyGetter albumArtistKey
  where
    headOrEmpty (x : _) = x
    headOrEmpty _ = mempty

albumArtistSetter :: AlbumArtist -> HTagLib.TagSetter
albumArtistSetter =
  HTagLib.propertySetter albumArtistKey . (: []) . unAlbumArtist
