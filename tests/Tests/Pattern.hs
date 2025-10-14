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
import Sound.HTagLib.Extra qualified as HTagLib
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
          (NonEmpty.fromList [NonEmpty.fromList [Pattern.FrText "some-path"]])
          (trackWithFile $ Path.Rel [relfile|./some-path|])
          `shouldBe` True,
      testCase "single file in fragments" $
        filenameMatchesNoFormatting
          ( NonEmpty.fromList
              [ NonEmpty.fromList
                  [ Pattern.FrText "some",
                    Pattern.FrText "-splitted",
                    Pattern.FrText "-path"
                  ]
              ]
          )
          (trackWithFile $ Path.Rel [relfile|./some-splitted-path|])
          `shouldBe` True,
      testCase "file in a directory" $
        filenameMatchesNoFormatting
          ( NonEmpty.fromList
              [ NonEmpty.fromList [Pattern.FrText "some-path"],
                NonEmpty.fromList [Pattern.FrText "to-somewhere"]
              ]
          )
          (trackWithFile $ Path.Rel [relfile|./some-path/to-somewhere|])
          `shouldBe` True,
      testCase "file in a directory with an extension" $
        filenameMatchesNoFormatting
          ( NonEmpty.fromList
              [ NonEmpty.fromList [Pattern.FrText "some-path"],
                NonEmpty.fromList [Pattern.FrText "to-somewhere"],
                NonEmpty.fromList [Pattern.FrText "audio"]
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
              [ NonEmpty.fromList
                  [Pattern.FrPlaceholder $ Pattern.PlTag Tag.Genre],
                NonEmpty.fromList
                  [Pattern.FrPlaceholder $ Pattern.PlTag Tag.Artist],
                NonEmpty.fromList
                  [Pattern.FrPlaceholder $ Pattern.PlTag Tag.Album],
                NonEmpty.fromList
                  [Pattern.FrPlaceholder $ Pattern.PlTag Tag.Title]
              ]
          )
          (trackWithFile $ Path.Rel [relfile|./genre/artist/album/title.mp3|])
          `shouldBe` True,
      testCase "multiple fragment per component" $
        filenameMatchesNoFormatting
          ( NonEmpty.fromList
              [ NonEmpty.fromList
                  [Pattern.FrPlaceholder $ Pattern.PlTag Tag.Genre],
                NonEmpty.fromList
                  [Pattern.FrPlaceholder $ Pattern.PlTag Tag.Artist],
                NonEmpty.fromList
                  [ Pattern.FrPlaceholder $ Pattern.PlTag Tag.Year,
                    Pattern.FrText "-",
                    Pattern.FrPlaceholder $ Pattern.PlTag Tag.Album
                  ],
                NonEmpty.fromList
                  [ Pattern.FrPlaceholder $ Pattern.PlTag Tag.Track,
                    Pattern.FrText "-",
                    Pattern.FrPlaceholder $ Pattern.PlTag Tag.Title
                  ]
              ]
          )
          ( trackWithFile $
              Path.Rel [relfile|./genre/artist/2024-album/1-title.mp3|]
          )
          `shouldBe` True,
      testCase "albumartist_ fallback to artist when albumartist is not present" $
        filenameMatchesNoFormatting
          ( NonEmpty.fromList
              [ NonEmpty.fromList
                  [Pattern.FrPlaceholder $ Pattern.PlTag Tag.Genre],
                NonEmpty.fromList
                  [Pattern.FrPlaceholder Pattern.PlAlbumArtist],
                NonEmpty.fromList
                  [Pattern.FrPlaceholder $ Pattern.PlTag Tag.Album],
                NonEmpty.fromList
                  [Pattern.FrPlaceholder $ Pattern.PlTag Tag.Title]
              ]
          )
          ( (trackWithFile $ Path.Rel [relfile|./genre/artist/album/title.mp3|])
              { AudioTrack.atAlbumArtist = ""
              }
          )
          `shouldBe` True,
      testCase "albumartist_ use albumartist when it is present" $
        filenameMatchesNoFormatting
          ( NonEmpty.fromList
              [ NonEmpty.fromList
                  [Pattern.FrPlaceholder $ Pattern.PlTag Tag.Genre],
                NonEmpty.fromList
                  [Pattern.FrPlaceholder Pattern.PlAlbumArtist],
                NonEmpty.fromList
                  [Pattern.FrPlaceholder $ Pattern.PlTag Tag.Album],
                NonEmpty.fromList
                  [Pattern.FrPlaceholder $ Pattern.PlTag Tag.Title]
              ]
          )
          (trackWithFile $ Path.Rel [relfile|./genre/albumartist/album/title.mp3|])
          `shouldBe` True
    ]

test :: TestTree
test =
  testGroup
    "Other patterns"
    [ testCase
        "fail if the filename is smaller than the pattern"
        $ ( filenameMatchesNoFormatting $
              NonEmpty.fromList
                [ NonEmpty.fromList [Pattern.FrText "some-path"],
                  NonEmpty.fromList
                    [Pattern.FrPlaceholder $ Pattern.PlTag Tag.Title]
                ]
          )
          ( trackWithFile $
              Path.Rel [relfile|./1-title.mp3|]
          )
          `shouldBe` False
    ]

test :: TestTree
test =
  testGroup
    "Pattern parser"
    [ testCase "single text" $
        Megaparsec.parse Pattern.parser "" "sometext"
          `shouldParse` NonEmpty.fromList
            [NonEmpty.fromList [Pattern.FrText "sometext"]],
      testCase "mixed text" $
        Megaparsec.parse Pattern.parser "" "sometext{artist}moretext"
          `shouldParse` NonEmpty.fromList
            [ NonEmpty.fromList
                [ Pattern.FrText "sometext",
                  Pattern.FrPlaceholder $ Pattern.PlTag Tag.Artist,
                  Pattern.FrText "moretext"
                ]
            ],
      testCase "some text" $
        Megaparsec.parse Pattern.parser "" "sometext/with/slash"
          `shouldParse` NonEmpty.fromList
            [ NonEmpty.fromList [Pattern.FrText "sometext"],
              NonEmpty.fromList [Pattern.FrText "with"],
              NonEmpty.fromList [Pattern.FrText "slash"]
            ],
      testCase "some mixed text" $
        Megaparsec.parse
          Pattern.parser
          ""
          "prefix-{artist}/{album}-suffix/before-{title}-after"
          `shouldParse` NonEmpty.fromList
            [ NonEmpty.fromList
                [ Pattern.FrText "prefix-",
                  Pattern.FrPlaceholder $ Pattern.PlTag Tag.Artist
                ],
              NonEmpty.fromList
                [ Pattern.FrPlaceholder $ Pattern.PlTag Tag.Album,
                  Pattern.FrText "-suffix"
                ],
              NonEmpty.fromList
                [ Pattern.FrText "before-",
                  Pattern.FrPlaceholder $ Pattern.PlTag Tag.Title,
                  Pattern.FrText "-after"
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
        "unwanted char"
        [ testCase "remove slashes" $
            filenameMatches
              trackDashTitle
              Pattern.noFormatting
              ( trackWithTitleAndFile "title/with/slashes" $
                  Path.Rel [relfile|./1-titlewithslashes.mp3|]
              )
              `shouldBe` True,
          testCase "slashes to underscore" $
            filenameMatches
              trackDashTitle
              ( Pattern.noFormatting
                  { Pattern.foUnwanted = [('/', Pattern.UnToUnderscore)]
                  }
              )
              ( trackWithTitleAndFile "title/with/slashes" $
                  Path.Rel [relfile|./1-title_with_slashes.mp3|]
              )
              `shouldBe` True,
          testCase "remove slashes, to underscore colons" $
            filenameMatches
              trackDashTitle
              ( Pattern.noFormatting
                  { Pattern.foUnwanted =
                      [ ('/', Pattern.UnRemove),
                        (':', Pattern.UnToUnderscore)
                      ]
                  }
              )
              ( trackWithTitleAndFile "title:with:colons/and/slash" $
                  Path.Rel [relfile|./1-title_with_colonsandslash.mp3|]
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
          (Pattern.noFormatting {Pattern.foPadTrackNumbers = Pattern.Pad 3})
          (trackWithTitleAndFile "title" $ Path.Rel [relfile|./001-title.mp3|])
          `shouldBe` True,
      testCase "ignore padding" $
        filenameMatches
          trackDashTitle
          (Pattern.noFormatting {Pattern.foPadTrackNumbers = Pattern.Ignore})
          ( (trackWithTitleAndFile "title" $ Path.Rel [relfile|./001-title.mp3|])
              { AudioTrack.atAlbumArtist = ""
              }
          )
          `shouldBe` True
    ]
  where
    trackDashTitle =
      NonEmpty.fromList
        [ NonEmpty.fromList
            [ Pattern.FrPlaceholder $ Pattern.PlTag Tag.Track,
              Pattern.FrText "-",
              Pattern.FrPlaceholder $ Pattern.PlTag Tag.Title
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
      atAlbumArtist = HTagLib.mkAlbumArtist "albumartist",
      atGenre = HTagLib.mkGenre "genre",
      atYear = HTagLib.mkYear 2024,
      atTrack = HTagLib.mkTrackNumber 1
    }

trackWithFile :: Path.SomeBase Path.File -> AudioTrack.AudioTrack
trackWithFile = trackWithTitleAndFile "title"
