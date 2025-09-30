{- AUTOCOLLECT.TEST -}
{-# LANGUAGE QuasiQuotes #-}

module Tests.Pattern
  (
  {- AUTOCOLLECT.TEST.export -}
  )
where

import AudioTrack qualified
import Check qualified
import Data.List.NonEmpty qualified as NonEmpty
import Path (relfile)
import Path qualified
import Pattern qualified
import Sound.HTagLib qualified as HTagLib
import Tag qualified
import Test.Hspec.Expectations (shouldBe)
import Test.Hspec.Megaparsec (shouldParse)
import Test.Tasty (testGroup)
import Test.Tasty.HUnit (testCase)
import Text.Megaparsec qualified as Megaparsec

test :: TestTree
test =
  testGroup
    "Static patterns"
    [ testCase "single file" $
        filenameMatches
          (NonEmpty.fromList [NonEmpty.fromList [Pattern.Text "some-path"]])
          (track $ Path.Rel [relfile|./some-path|])
          `shouldBe` True,
      testCase "single file in parts" $
        filenameMatches
          ( NonEmpty.fromList
              [ NonEmpty.fromList
                  [ Pattern.Text "some",
                    Pattern.Text "-splitted",
                    Pattern.Text "-path"
                  ]
              ]
          )
          (track $ Path.Rel [relfile|./some-splitted-path|])
          `shouldBe` True,
      testCase "file in a directory" $
        filenameMatches
          ( NonEmpty.fromList
              [ NonEmpty.fromList [Pattern.Text "some-path"],
                NonEmpty.fromList [Pattern.Text "to-somewhere"]
              ]
          )
          (track $ Path.Rel [relfile|./some-path/to-somewhere|])
          `shouldBe` True,
      testCase "file in a directory with an extension" $
        filenameMatches
          ( NonEmpty.fromList
              [ NonEmpty.fromList [Pattern.Text "some-path"],
                NonEmpty.fromList [Pattern.Text "to-somewhere"],
                NonEmpty.fromList [Pattern.Text "audio"]
              ]
          )
          (track $ Path.Rel [relfile|./some-path/to-somewhere/audio.mp3|])
          `shouldBe` True
    ]

test :: TestTree
test =
  testGroup
    "Tag patterns"
    [ testCase "one item per piece" $
        filenameMatches
          ( NonEmpty.fromList
              [ NonEmpty.fromList [Pattern.Placeholder Tag.Genre],
                NonEmpty.fromList [Pattern.Placeholder Tag.Artist],
                NonEmpty.fromList [Pattern.Placeholder Tag.Album],
                NonEmpty.fromList [Pattern.Placeholder Tag.Title]
              ]
          )
          (track $ Path.Rel [relfile|./genre/artist/album/title.mp3|])
          `shouldBe` True,
      testCase "multiple items per piece" $
        filenameMatches
          ( NonEmpty.fromList
              [ NonEmpty.fromList [Pattern.Placeholder Tag.Genre],
                NonEmpty.fromList [Pattern.Placeholder Tag.Artist],
                NonEmpty.fromList
                  [ Pattern.Placeholder Tag.Year,
                    Pattern.Text "-",
                    Pattern.Placeholder Tag.Album
                  ],
                NonEmpty.fromList
                  [ Pattern.Placeholder Tag.Track,
                    Pattern.Text "-",
                    Pattern.Placeholder Tag.Title
                  ]
              ]
          )
          (track $ Path.Rel [relfile|./genre/artist/2024-album/1-title.mp3|])
          `shouldBe` True
    ]

test :: TestTree
test =
  testGroup
    "Pattern parser"
    [ testCase "single text" $
        Megaparsec.parse Pattern.parser "" "sometext"
          `shouldParse` NonEmpty.fromList
            [NonEmpty.fromList [Pattern.Text "sometext"]],
      testCase "mixed text" $
        Megaparsec.parse Pattern.parser "" "sometext{artist}moretext"
          `shouldParse` NonEmpty.fromList
            [ NonEmpty.fromList
                [ Pattern.Text "sometext",
                  Pattern.Placeholder Tag.Artist,
                  Pattern.Text "moretext"
                ]
            ],
      testCase "some text" $
        Megaparsec.parse Pattern.parser "" "sometext/with/slash"
          `shouldParse` NonEmpty.fromList
            [ NonEmpty.fromList [Pattern.Text "sometext"],
              NonEmpty.fromList [Pattern.Text "with"],
              NonEmpty.fromList [Pattern.Text "slash"]
            ],
      testCase "some mixed text" $
        Megaparsec.parse
          Pattern.parser
          ""
          "prefix-{artist}/{album}-suffix/before-{title}-after"
          `shouldParse` NonEmpty.fromList
            [ NonEmpty.fromList
                [ Pattern.Text "prefix-",
                  Pattern.Placeholder Tag.Artist
                ],
              NonEmpty.fromList
                [ Pattern.Placeholder Tag.Album,
                  Pattern.Text "-suffix"
                ],
              NonEmpty.fromList
                [ Pattern.Text "before-",
                  Pattern.Placeholder Tag.Title,
                  Pattern.Text "-after"
                ]
            ]
    ]

filenameMatches :: Pattern.Pattern -> AudioTrack.AudioTrack -> Bool
filenameMatches pattern track' =
  isRight $ Check.check (Check.FilenameMatches pattern) track'

track :: Path.SomeBase Path.File -> AudioTrack.AudioTrack
track file =
  AudioTrack.AudioTrack
    { atFile = file,
      atTitle = HTagLib.mkTitle "title",
      atArtist = HTagLib.mkArtist "artist",
      atAlbum = HTagLib.mkAlbum "album",
      atGenre = HTagLib.mkGenre "genre",
      atYear = HTagLib.mkYear 2024,
      atTrack = HTagLib.mkTrackNumber 1
    }
