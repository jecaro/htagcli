module Tests.MusicBrainz.Types (test) where

import Data.Aeson qualified as Aeson
import Data.List.NonEmpty qualified as NonEmpty
import Data.UUID qualified as UUID
import MusicBrainz.Types qualified as MusicBrainz
import Relude.Unsafe qualified as Unsafe
import Test.Hspec.Expectations (shouldBe)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit qualified as Tasty

test :: Tasty.TestTree
test = Tasty.testGroup "MusicBrainz.Types" [testParseJSON]

testParseJSON :: Tasty.TestTree
testParseJSON =
  Tasty.testGroup
    "JSON parsing"
    [ -- curl "https://musicbrainz.org/ws/2/release?query=artist:%22fugazi%22+AND+release:%22repeater%22&fmt=json&limit=1" | jq > ./data/musicbrainz-repeater-search.json
      Tasty.testCase "parse release from search" $ do
        result <- Aeson.eitherDecodeFileStrict "data/musicbrainz-repeater-search.json"
        result
          `shouldBe` Right
            MusicBrainz.SearchResponse
              { MusicBrainz.srReleases =
                  [ MusicBrainz.Release
                      { MusicBrainz.reId =
                          Unsafe.fromJust $
                            UUID.fromString "37e6a462-1417-45dc-9d88-4ef9aff4bc19",
                        MusicBrainz.reTitle = "Repeater",
                        MusicBrainz.reArtistCredit = mkArtistCredit "Fugazi",
                        MusicBrainz.reDate = Just 2005,
                        MusicBrainz.reTrackCount = 14,
                        MusicBrainz.reScore = 100
                      }
                  ]
              },
      -- curl "https://musicbrainz.org/ws/2/release?query=release:%22Dischord+1981%3A+The+Year+in+Seven+Inches%22&fmt=json&limit=1" | jq > ./data/musicbrainz-dischord-search.json
      Tasty.testCase "parse compilation release" $ do
        result <- Aeson.eitherDecodeFileStrict "data/musicbrainz-dischord-search.json"
        result
          `shouldBe` Right
            MusicBrainz.SearchResponse
              { MusicBrainz.srReleases =
                  [ MusicBrainz.Release
                      { MusicBrainz.reId =
                          Unsafe.fromJust $
                            UUID.fromString $
                              "2b06e322-88e4-465c-b53d-1f82271e6131",
                        MusicBrainz.reTitle = "Dischord 1981: The Year in Seven Inches",
                        MusicBrainz.reArtistCredit = mkArtistCredit "Various Artists",
                        MusicBrainz.reDate = Just 1995,
                        MusicBrainz.reTrackCount = 48,
                        MusicBrainz.reScore = 100
                      }
                  ]
              },
      -- curl "https://musicbrainz.org/ws/2/release/00baa173-29db-33a9-af6d-fe109e53a211?inc=recordings+artist-credits&fmt=json" | jq > ./data/musicbrainz-repeater-detail.json
      Tasty.testCase "parse release detail" $ do
        result <- Aeson.eitherDecodeFileStrict "data/musicbrainz-repeater-detail.json"
        result
          `shouldBe` Right
            MusicBrainz.ReleaseDetail
              { MusicBrainz.rdId = "00baa173-29db-33a9-af6d-fe109e53a211",
                MusicBrainz.rdTitle = "Repeater",
                MusicBrainz.rdArtistCredit = mkArtistCredit "Fugazi",
                MusicBrainz.rdDate = Just 1990,
                MusicBrainz.rdMedia =
                  MusicBrainz.Media
                    { MusicBrainz.mePosition = 1,
                      MusicBrainz.meTracks =
                        NonEmpty.zipWith
                          mkTrack
                          (1 :| [2 ..])
                          ( "Turnover"
                              :| [ "Repeater",
                                   "Brendan #1",
                                   "Merchandise",
                                   "Blueprint",
                                   "Sieve-Fisted Find",
                                   "Greed",
                                   "Two Beats Off",
                                   "Styrofoam",
                                   "Reprovisional",
                                   "Shut the Door"
                                 ]
                          )
                    }
                    :| []
              }
    ]

mkArtistCredit :: Text -> NonEmpty MusicBrainz.ArtistCredit
mkArtistCredit name =
  MusicBrainz.ArtistCredit
    { MusicBrainz.acName = name,
      MusicBrainz.acJoinphrase = Nothing
    }
    :| []

mkTrack :: Int -> Text -> MusicBrainz.Track
mkTrack n title =
  MusicBrainz.Track
    { MusicBrainz.trPosition = n,
      MusicBrainz.trRecording =
        MusicBrainz.Recording
          { MusicBrainz.rcTitle = title,
            MusicBrainz.rcArtistCredit = mkArtistCredit "Fugazi"
          }
    }
