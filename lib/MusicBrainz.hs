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
import Data.Text.Metrics qualified as Metrics
import Data.Version qualified as Version
import Model.Album qualified as Album
import Model.AudioTrack qualified as AudioTrack
import Model.Disc qualified as Disc
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

artistCreditToText :: NonEmpty ArtistCredit -> Text
artistCreditToText = foldMap (\ArtistCredit {..} -> acName <> fromMaybe "" acJoinphrase)

data Average = Average
  { avSum :: Double,
    avCount :: Int
  }

instance Semigroup Average where
  Average s1 n1 <> Average s2 n2 = Average (s1 + s2) (n1 + n2)

instance Monoid Average where
  mempty = Average 0 0

averageToDouble :: Average -> Double
averageToDouble Average {..}
  | avCount == 0 = 1
  | otherwise = avSum / fromIntegral avCount

similarityDisc :: Media -> Disc.Disc -> Average
similarityDisc Media {..} disc =
  Average
    { avSum = sum (zipWith similarity lts found),
      avCount = max (length lts) (length found)
    }
  where
    found = toList $ rcTitle . trRecording <$> meTracks
    lts = toList $ HTagLib.unTitle . AudioTrack.atTitle <$> Disc.tracks disc

-- | Weighted similarity between a local album and a MusicBrainz release detail.
-- Weights: artist 20%, title 20%, tracks from discs 60%.
similarityAlbum :: ReleaseDetail -> Album.Album -> Double
similarityAlbum detail album =
  0.2 * artistSimilarity + 0.2 * titleSimilarity + 0.6 * discsSimilarity
  where
    artistSimilarity =
      similarity
        (HTagLib.unAlbumArtist $ Album.albumArtist album)
        (artistCreditToText $ rdArtistCredit detail)
    titleSimilarity =
      similarity
        (HTagLib.unAlbum $ Album.album album)
        (rdTitle detail)
    discsSimilarity =
      averageToDouble $
        foldMap (uncurry similarityDisc) $
          zip (toList $ rdMedia detail) (toList $ Album.discs album)

similarity :: Text -> Text -> Double
similarity a b =
  realToFrac $ Metrics.jaroWinkler (Text.toLower a) (Text.toLower b)

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
  search maxResults (Album.albumArtist album) (Album.album album) (Just album)

search ::
  Int -> HTagLib.AlbumArtist -> HTagLib.Album -> Maybe Album.Album -> IO ()
search maxResults albumArtist album mbLocalAlbum = do
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
    displayRelease idx (release, detail) mbLocalAlbum
  where
    albumArtistText = HTagLib.unAlbumArtist albumArtist
    albumText = HTagLib.unAlbum album

displayRelease :: Int -> (Release, ReleaseDetail) -> Maybe Album.Album -> IO ()
displayRelease idx (Release {..}, detail) mbAlbum = do
  putTextLn
    [__i|
      #{idx}. ID: #{reId} #{overallSuffix}
         Artist: #{artist} #{artistSuffix}
         Album: #{reTitle} #{titleSuffix}
         Year: #{year} #{yearSuffix}
         Discs: #{mediaCount} #{mediaCountSuffix}
         Tracks: #{trackCount} #{trackCountSuffix}
      |]
  putTextLn ""

  traverse_ (uncurry displayMedia) mediaAndDiscs
  where
    medias = toList $ rdMedia detail
    discs = orMempty (fmap Just . toList . Album.discs) mbAlbum
    mediaAndDiscs = zip medias (discs <> repeat Nothing)

    overallSuffix = inParensMaybe (percentage . similarityAlbum detail) mbAlbum

    artist = artistCreditToText reArtistCredit
    localArtist = HTagLib.unAlbumArtist . Album.albumArtist <$> mbAlbum
    artistSuffix = orMempty (similaritySuffix artist) localArtist

    localTitle = HTagLib.unAlbum . Album.album <$> mbAlbum
    titleSuffix = orMempty (similaritySuffix reTitle) localTitle

    year :: Text
    year = maybe "unknown" show reDate
    localYears = orMempty (fmap HTagLib.unYear . Album.years) mbAlbum
    yearSuffix
      | null localYears || localYears == maybeToList reDate = ""
      | otherwise = inParens $ Text.intercalate ", " $ show <$> localYears

    localDiscCount = length . Album.discs <$> mbAlbum
    mediaCount = length $ rdMedia detail
    mediaCountSuffix = orMempty (showIfDifferent mediaCount) localDiscCount

    trackCount = sum $ length . meTracks <$> toList (rdMedia detail)
    localTrackCount = length . (Disc.tracks <=< Album.discs) <$> mbAlbum
    trackCountSuffix = orMempty (showIfDifferent trackCount) localTrackCount

displayMedia :: Media -> Maybe Disc.Disc -> IO ()
displayMedia media@Media {..} mDisc = do
  putTextLn [i|   Disc #{mePosition}: #{discSimilarity} - Tracks: #{trackCount} #{trackCountSuffix}|]
  putTextLn ""

  forM_ tracksAndLocalTracks $ \(Track {..}, mLocalTitle) -> do
    let trackSuffix = orMempty (similaritySuffix (rcTitle trRecording)) mLocalTitle
    putTextLn [i|     #{trPosition}. #{rcTitle trRecording} #{trackSuffix}|]

  putTextLn ""
  where
    tracks = toList meTracks
    tracksAndLocalTracks = zip tracks $ localTitles <> repeat Nothing

    discSimilarity =
      inParensMaybe (percentage . averageToDouble . similarityDisc media) mDisc

    trackCount = length tracks
    localTrackCount = length . Disc.tracks <$> mDisc
    trackCountSuffix = orMempty (showIfDifferent trackCount) localTrackCount

    localTitles =
      orMempty
        (fmap (Just . HTagLib.unTitle . AudioTrack.atTitle) . toList . Disc.tracks)
        mDisc

percentage :: Double -> Text
percentage value = show (round (value * 100) :: Int) <> "%"

displaySimilarity :: Double -> Text -> Text
displaySimilarity value text =
  inParens $
    percentage value <> if value < 0.95 then " - " <> text else ""

inParens :: Text -> Text
inParens text = "(" <> text <> ")"

showIfDifferent :: (Show a, Eq a) => a -> a -> Text
showIfDifferent mbValue localValue
  | localValue == mbValue = ""
  | otherwise = inParens (show localValue)

similaritySuffix :: Text -> Text -> Text
similaritySuffix searchText localText =
  displaySimilarity (similarity searchText localText) localText

inParensMaybe :: (a -> Text) -> Maybe a -> Text
inParensMaybe aToText mbText = orMempty (inParens . aToText) mbText

orMempty :: (Monoid m) => (a -> m) -> Maybe a -> m
orMempty = maybe mempty
