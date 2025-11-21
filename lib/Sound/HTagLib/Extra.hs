module Sound.HTagLib.Extra
  ( AlbumArtist,
    mkAlbumArtist,
    unAlbumArtist,
    albumArtistGetter,
    albumArtistSetter,
    DiscNumber,
    mkDiscNumber,
    unDiscNumber,
    discNumberGetter,
    discNumberSetter,
  )
where

import Data.Text qualified as Text
import Sound.HTagLib qualified as HTagLib

newtype AlbumArtist = AlbumArtist Text
  deriving (Show, Eq)

instance IsString AlbumArtist where
  fromString = mkAlbumArtist . fromString

newtype DiscNumber = DiscNumber Int
  deriving (Show, Eq)

unAlbumArtist :: AlbumArtist -> Text
unAlbumArtist (AlbumArtist albumArtist) = albumArtist

mkAlbumArtist :: Text -> AlbumArtist
mkAlbumArtist = AlbumArtist . Text.map nullToSpace
  where
    nullToSpace '\0' = ' '
    nullToSpace c = c

unDiscNumber :: DiscNumber -> Int
unDiscNumber (DiscNumber disc) = disc

mkDiscNumber :: Int -> Maybe DiscNumber
mkDiscNumber n
  | n > 0 = Just (DiscNumber n)
  | otherwise = Nothing

albumArtistKey :: Text
albumArtistKey = "ALBUMARTIST"

albumArtistGetter :: HTagLib.TagGetter AlbumArtist
albumArtistGetter =
  mkAlbumArtist . headOrMempty <$> HTagLib.propertyGetter albumArtistKey

albumArtistSetter :: AlbumArtist -> HTagLib.TagSetter
albumArtistSetter =
  HTagLib.propertySetter albumArtistKey . (: []) . unAlbumArtist

discNumberKey :: Text
discNumberKey = "DISCNUMBER"

discNumberGetter :: HTagLib.TagGetter (Maybe DiscNumber)
discNumberGetter =
  toDiscNumber . headOrMempty <$> HTagLib.propertyGetter discNumberKey
  where
    toDiscNumber =
      mkDiscNumber <=< readMaybe . toString . fst . Text.break (== '/')

discNumberSetter :: Maybe DiscNumber -> HTagLib.TagSetter
discNumberSetter =
  HTagLib.propertySetter discNumberKey
    . (: [])
    . maybe mempty (show . unDiscNumber)

headOrMempty :: (Monoid a) => [a] -> a
headOrMempty (x : _) = x
headOrMempty _ = mempty
