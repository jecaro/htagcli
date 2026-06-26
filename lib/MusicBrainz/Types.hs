module MusicBrainz.Types
  ( ArtistCredit (..),
    Media (..),
    Recording (..),
    Release (..),
    ReleaseDetail (..),
    SearchResponse (..),
    Track (..),
    artistCreditToText,
  )
where

import Data.Aeson ((.:), (.:?))
import Data.Aeson qualified as Aeson
import Data.Text qualified as Text

data ArtistCredit = ArtistCredit
  { acName :: Text,
    acJoinphrase :: Maybe Text
  }
  deriving (Show, Eq)

-- | A release from MusicBrainz search results
data Release = Release
  { reId :: Text,
    reTitle :: Text,
    reArtistCredit :: NonEmpty ArtistCredit,
    -- | Year parsed from MusicBrainz date field (format: YYYY, YYYY-MM, or
    -- YYYY-MM-DD); absent or empty on some releases
    reDate :: Maybe Int,
    reTrackCount :: Int,
    -- | Lucene relevance score (0-100), higher means closer match to the query
    reScore :: Int
  }
  deriving (Show, Eq)

data Recording = Recording
  { rcTitle :: Text,
    rcArtistCredit :: NonEmpty ArtistCredit
  }
  deriving (Show, Eq)

-- | A track from MusicBrainz release lookup
data Track = Track
  { trPosition :: Int,
    trRecording :: Recording
  }
  deriving (Show, Eq)

data Media = Media
  { mePosition :: Int,
    meTracks :: NonEmpty Track
  }
  deriving (Show, Eq)

-- | Full release details from lookup
data ReleaseDetail = ReleaseDetail
  { rdId :: Text,
    rdTitle :: Text,
    rdArtistCredit :: NonEmpty ArtistCredit,
    rdDate :: Maybe Int,
    rdMedia :: NonEmpty Media
  }
  deriving (Show, Eq)

data SearchResponse = SearchResponse
  { srReleases :: [Release]
  }
  deriving (Show, Eq)

instance Aeson.FromJSON ArtistCredit where
  parseJSON = Aeson.withObject "ArtistCredit" $ \o -> do
    acName <- o .: "name"
    joinphrase <- o .:? "joinphrase"
    let acJoinphrase = if joinphrase == Just "" then Nothing else joinphrase
    pure ArtistCredit {..}

instance Aeson.FromJSON Release where
  parseJSON = Aeson.withObject "Release" $ \o -> do
    reId <- o .: "id"
    reTitle <- o .: "title"
    reArtistCredit <- o .: "artist-credit"
    dateText <- o .:? "date"
    let reDate = dateText >>= parseDate
    reTrackCount <- o .: "track-count"
    reScore <- o .: "score"
    pure Release {..}

instance Aeson.FromJSON SearchResponse where
  parseJSON = Aeson.withObject "SearchResponse" $ \o -> do
    srReleases <- o .: "releases"
    pure SearchResponse {..}

instance Aeson.FromJSON ReleaseDetail where
  parseJSON = Aeson.withObject "ReleaseDetail" $ \o -> do
    rdId <- o .: "id"
    rdTitle <- o .: "title"
    rdArtistCredit <- o .: "artist-credit"
    dateText <- o .:? "date"
    let rdDate = dateText >>= parseDate
    rdMedia <- o .: "media"
    pure ReleaseDetail {..}

instance Aeson.FromJSON Media where
  parseJSON = Aeson.withObject "Media" $ \o -> do
    mePosition <- o .: "position"
    meTracks <- o .: "tracks"
    pure Media {..}

instance Aeson.FromJSON Track where
  parseJSON = Aeson.withObject "Track" $ \o -> do
    trPosition <- o .: "position"
    trRecording <- o .: "recording"
    pure Track {..}

instance Aeson.FromJSON Recording where
  parseJSON = Aeson.withObject "Recording" $ \o -> do
    rcTitle <- o .: "title"
    rcArtistCredit <- o .: "artist-credit"
    pure Recording {..}

parseDate :: Text -> Maybe Int
parseDate = readMaybe . toString . Text.take 4

artistCreditToText :: NonEmpty ArtistCredit -> Text
artistCreditToText =
  foldMap (\ArtistCredit {..} -> acName <> fromMaybe "" acJoinphrase)
