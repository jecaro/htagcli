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
        filenameMatchesNoFormatting
          (NonEmpty.fromList [NonEmpty.fromList [Pattern.Text "some-path"]])
          (trackWithFile $ Path.Rel [relfile|./some-path|])
          `shouldBe` True,
      testCase "single file in fragments" $
        filenameMatchesNoFormatting
          ( NonEmpty.fromList
              [ NonEmpty.fromList
                  [ Pattern.Text "some",
                    Pattern.Text "-splitted",
                    Pattern.Text "-path"
                  ]
              ]
          )
          (trackWithFile $ Path.Rel [relfile|./some-splitted-path|])
          `shouldBe` True,
      testCase "file in a directory" $
        filenameMatchesNoFormatting
          ( NonEmpty.fromList
              [ NonEmpty.fromList [Pattern.Text "some-path"],
                NonEmpty.fromList [Pattern.Text "to-somewhere"]
              ]
          )
          (trackWithFile $ Path.Rel [relfile|./some-path/to-somewhere|])
          `shouldBe` True,
      testCase "file in a directory with an extension" $
        filenameMatchesNoFormatting
          ( NonEmpty.fromList
              [ NonEmpty.fromList [Pattern.Text "some-path"],
                NonEmpty.fromList [Pattern.Text "to-somewhere"],
                NonEmpty.fromList [Pattern.Text "audio"]
              ]
          )
          (trackWithFile $ Path.Rel [relfile|./some-path/to-somewhere/audio.mp3|])
          `shouldBe` True
    ]

test :: TestTree
test =
  testGroup
    "Tag patterns"
    [ testCase "one fragment per component" $
        filenameMatchesNoFormatting
          ( NonEmpty.fromList
              [ NonEmpty.fromList [Pattern.Placeholder Tag.Genre],
                NonEmpty.fromList [Pattern.Placeholder Tag.Artist],
                NonEmpty.fromList [Pattern.Placeholder Tag.Album],
                NonEmpty.fromList [Pattern.Placeholder Tag.Title]
              ]
          )
          (trackWithFile $ Path.Rel [relfile|./genre/artist/album/title.mp3|])
          `shouldBe` True,
      testCase "multiple fragment per component" $
        filenameMatchesNoFormatting
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
          ( trackWithFile $
              Path.Rel [relfile|./genre/artist/2024-album/1-title.mp3|]
          )
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

test :: TestTree
test =
  testGroup
    "Formatting options"
    [ testCase "no substitutions" $
        filenameMatches
          trackDashTitle
          Pattern.noFormatting
          (trackWithTitleAndFile "title" $ Path.Rel [relfile|./1-title.mp3|])
          `shouldBe` True,
      testGroup
        "tag with slashes"
        [ testCase "remove" $
            filenameMatches
              trackDashTitle
              (Pattern.noFormatting {Pattern.foSlashes = Pattern.SlRemove})
              ( trackWithTitleAndFile "title/with/slashes" $
                  Path.Rel [relfile|./1-titlewithslashes.mp3|]
              )
              `shouldBe` True,
          testCase "to underscore" $
            filenameMatches
              trackDashTitle
              (Pattern.noFormatting {Pattern.foSlashes = Pattern.SlToUnderscore})
              ( trackWithTitleAndFile "title/with/slashes" $
                  Path.Rel [relfile|./1-title_with_slashes.mp3|]
              )
              `shouldBe` True
        ],
      testGroup
        "tag with spaces"
        [ testCase "keep" $
            filenameMatches
              trackDashTitle
              (Pattern.noFormatting {Pattern.foSpaces = Pattern.SpKeep})
              ( trackWithTitleAndFile "title with spaces" $
                  Path.Rel [relfile|./1-title with spaces.mp3|]
              )
              `shouldBe` True,
          testCase "to underscore" $
            filenameMatches
              trackDashTitle
              (Pattern.noFormatting {Pattern.foSpaces = Pattern.SpToUnderscore})
              ( trackWithTitleAndFile "title with spaces" $
                  Path.Rel [relfile|./1-title_with_spaces.mp3|]
              )
              `shouldBe` True
        ],
      testCase "zero padding" $
        filenameMatches
          trackDashTitle
          (Pattern.noFormatting {Pattern.foPadTrackNumbers = 3})
          ( trackWithTitleAndFile "title" $
              Path.Rel [relfile|./001-title.mp3|]
          )
          `shouldBe` True
    ]
  where
    trackDashTitle =
      NonEmpty.fromList
        [ NonEmpty.fromList
            [ Pattern.Placeholder Tag.Track,
              Pattern.Text "-",
              Pattern.Placeholder Tag.Title
            ]
        ]

filenameMatchesNoFormatting :: Pattern.Pattern -> AudioTrack.AudioTrack -> Bool
filenameMatchesNoFormatting pattern = filenameMatches pattern Pattern.noFormatting

filenameMatches ::
  Pattern.Pattern -> Pattern.Formatting -> AudioTrack.AudioTrack -> Bool
filenameMatches pattern formatting =
  isRight . Check.check (Check.FilenameMatches pattern formatting)

trackWithTitleAndFile :: Text -> Path.SomeBase Path.File -> AudioTrack.AudioTrack
trackWithTitleAndFile title file =
  AudioTrack.AudioTrack
    { atFile = file,
      atTitle = HTagLib.mkTitle title,
      atArtist = HTagLib.mkArtist "artist",
      atAlbum = HTagLib.mkAlbum "album",
      atGenre = HTagLib.mkGenre "genre",
      atYear = HTagLib.mkYear 2024,
      atTrack = HTagLib.mkTrackNumber 1
    }

trackWithFile :: Path.SomeBase Path.File -> AudioTrack.AudioTrack
trackWithFile = trackWithTitleAndFile "title"
