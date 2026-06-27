{-# LANGUAGE QuasiQuotes #-}

module MusicBrainz.Req
  ( lookupRelease,
    searchReleases,
  )
where

import Data.Aeson qualified as Aeson
import Data.String.Interpolate (i)
import Data.UUID qualified as UUID
import Data.Version qualified as Version
import MusicBrainz.Types qualified as MusicBrainz
import Network.HTTP.Req ((/:), (=:))
import Network.HTTP.Req qualified as Req
import Paths_htagcli qualified as Paths
import Sound.HTagLib qualified as HTagLib
import Sound.HTagLib.Extra qualified as HTagLib

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
searchReleases ::
  Int -> HTagLib.AlbumArtist -> HTagLib.Album -> IO [MusicBrainz.Release]
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
  MusicBrainz.srReleases <$> Aeson.throwDecodeStrict (Req.responseBody response)
  where
    albumArtistText = HTagLib.unAlbumArtist albumArtist
    albumText = HTagLib.unAlbum album
    query :: Text
    query = [i|artist:"#{albumArtistText}" AND release:"#{albumText}"|]

-- | Lookup a release by MBID
lookupRelease :: UUID.UUID -> IO MusicBrainz.ReleaseDetail
lookupRelease releaseId = do
  response <-
    Req.runReq Req.defaultHttpConfig $
      Req.req
        Req.GET
        (baseUrl /: "release" /: UUID.toText releaseId)
        Req.NoReqBody
        Req.bsResponse
        ( headers
            <> "inc" =: ("recordings+artist-credits" :: Text)
            <> "fmt" =: ("json" :: Text)
        )
  Aeson.throwDecodeStrict $ Req.responseBody response
