{-# LANGUAGE QuasiQuotes #-}

module MusicBrainz
  ( ArtistCredit (..),
    Media (..),
    Recording (..),
    Release (..),
    ReleaseDetail (..),
    Track (..),
    SearchResponse (..),
    search,
    searchAlbum,
  )
where

import Control.Concurrent qualified as Concurrent
import Data.Aeson ((.:), (.:?))
import Data.Aeson qualified as Aeson
import Data.String.Interpolate (i, __i)
import Data.Text qualified as Text
import Data.Version qualified as Version
import Model.Album qualified as Album
import Network.HTTP.Req ((/:), (=:))
import Network.HTTP.Req qualified as Req
import Paths_htagcli qualified as Paths
import Sound.HTagLib qualified as HTagLib
import Sound.HTagLib.Extra qualified as HTagLib

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

parseDate :: Text -> Maybe Int
parseDate = readMaybe . toString . Text.take 4

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

data SearchResponse = SearchResponse
  { srReleases :: [Release]
  }
  deriving (Show, Eq)

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

baseUrl :: Req.Url Req.Https
baseUrl = Req.https "musicbrainz.org" /: "ws" /: "2"

headers :: Req.Option scheme
headers =
  Req.header "User-Agent" userAgent <> Req.header "Accept" "application/json"
  where
    userAgent :: ByteString
    userAgent = [i|htagcli/#{version} ( #{url} )|]
    version :: Text
    version = fromString $ Version.showVersion Paths.version
    url :: Text
    url = "https://github.com/jecaro/htagcli"

-- | Search for releases on MusicBrainz
searchReleases :: Int -> HTagLib.AlbumArtist -> HTagLib.Album -> IO [Release]
searchReleases limit albumArtist album = do
  response <-
    Req.runReq Req.defaultHttpConfig $
      Req.req
        Req.GET
        (baseUrl /: "release")
        Req.NoReqBody
        Req.bsResponse
        ( headers
            <> "query" =: query
            <> "fmt" =: ("json" :: Text)
            <> "limit" =: limit
        )
  srReleases <$> Aeson.throwDecodeStrict (Req.responseBody response)
  where
    albumArtistText = HTagLib.unAlbumArtist albumArtist
    albumText = HTagLib.unAlbum album
    query :: Text
    query = [i|artist:"#{albumArtistText}" AND release:"#{albumText}"|]

-- | Lookup a release by MBID
lookupRelease :: Text -> IO ReleaseDetail
lookupRelease mbid = do
  response <-
    Req.runReq Req.defaultHttpConfig $
      Req.req
        Req.GET
        (baseUrl /: "release" /: mbid)
        Req.NoReqBody
        Req.bsResponse
        ( headers
            <> "inc" =: ("recordings+artist-credits" :: Text)
            <> "fmt" =: ("json" :: Text)
        )
  Aeson.throwDecodeStrict $ Req.responseBody response

searchAlbum :: Int -> Album.Album -> IO ()
searchAlbum maxResults album =
  search maxResults (Album.albumArtist album) (Album.album album)

search :: Int -> HTagLib.AlbumArtist -> HTagLib.Album -> IO ()
search maxResults albumArtist album = do
  putTextLn [i|Searching: "#{albumArtistText}" - "#{albumText}"|]
  putTextLn ""

  releases <- searchReleases maxResults albumArtist album
  let nbReleases = length releases
      s :: Text
      s = if nbReleases > 1 then "s" else ""
  putTextLn [i|#{nbReleases} release#{s} found\n|]

  forM_ (zip [1 ..] releases) $ \(idx, release) -> do
    -- Official MusicBrainz API rate limit
    Concurrent.threadDelay 1_000_000
    detail <- lookupRelease $ reId release
    displayRelease idx (release, detail)
    putTextLn ""
  where
    albumArtistText = HTagLib.unAlbumArtist albumArtist
    albumText = HTagLib.unAlbum album

displayRelease :: Int -> (Release, ReleaseDetail) -> IO ()
displayRelease idx (Release {..}, detail) = do
  let artist = artistCreditToText reArtistCredit
      date :: Text
      date = maybe "unknown" show reDate
      trackCount = sum $ fmap (length . meTracks) $ toList $ rdMedia detail
  putTextLn
    [__i|
      #{idx}. #{reTitle} by #{artist}
         ID: #{reId}
         Date: #{date}
         Tracks: #{trackCount}
      |]

  forM_ (rdMedia detail) $ \Media {..} -> do
    putTextLn [i|   Disc #{mePosition}:|]
    forM_ meTracks $ \Track {..} ->
      putTextLn [i|     #{trPosition}. #{rcTitle trRecording}|]
  where
    artistCreditToText :: NonEmpty ArtistCredit -> Text
    artistCreditToText =
      foldMap (\ArtistCredit {..} -> acName <> joinphraseToText acJoinphrase)
    joinphraseToText :: Maybe Text -> Text
    joinphraseToText = fromMaybe ""
