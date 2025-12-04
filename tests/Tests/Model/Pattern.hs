{- AUTOCOLLECT.TEST -}
{-# LANGUAGE QuasiQuotes #-}

module Tests.Model.Pattern
  (
  {- AUTOCOLLECT.TEST.export -}
  )
where

import Check.Track qualified as Track
import Hedgehog ((===))
import Hedgehog qualified
import Hedgehog.Gen qualified as HedgehogGen
import Hedgehog.Range qualified as HedgehogRange
import Model.AudioTrack qualified as AudioTrack
import Model.Pattern qualified as Pattern
import Model.Tag qualified as Tag
import Path (absdir, absfile, (</>))
import Path qualified
import Sound.HTagLib qualified as HTagLib
import Sound.HTagLib.Extra qualified as HTagLib
import Test.Hspec.Expectations (shouldBe)
import Test.Hspec.Megaparsec (shouldParse)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit qualified as Tasty
import Test.Tasty.Hedgehog qualified as Tasty
import Text.Megaparsec qualified as Megaparsec

patternGen :: Hedgehog.Gen Pattern.Pattern
patternGen = HedgehogGen.nonEmpty (HedgehogRange.linear 1 5) componentGen

componentGen :: Hedgehog.Gen Pattern.Component
componentGen =
  concatConsecutiveTexts
    <$> HedgehogGen.nonEmpty (HedgehogRange.linear 1 5) placeholderGen

concatConsecutiveTexts :: NonEmpty Pattern.Fragment -> NonEmpty Pattern.Fragment
concatConsecutiveTexts ((Pattern.FrText txt1) :| ((Pattern.FrText txt2) : xs)) =
  concatConsecutiveTexts (Pattern.FrText (txt1 <> txt2) :| xs)
concatConsecutiveTexts (fragments :| (x : xs)) =
  fragments :| toList (concatConsecutiveTexts $ x :| xs)
concatConsecutiveTexts fragments = fragments

placeholderGen :: Hedgehog.Gen Pattern.Fragment
placeholderGen =
  HedgehogGen.choice
    [ Pattern.FrPlaceholder
        <$> HedgehogGen.choice
          [ Pattern.PlTag <$> HedgehogGen.enumBounded,
            Pattern.PlTagOr
              <$> HedgehogGen.enumBounded
              <*> HedgehogGen.enumBounded,
            Pattern.PlOptional
              <$> textGen
              <*> HedgehogGen.enumBounded
              <*> textGen
          ],
      Pattern.FrText <$> textGen
    ]

textGen :: Hedgehog.Gen Text
textGen =
  HedgehogGen.text (HedgehogRange.linear 1 10) HedgehogGen.alphaNum

test :: TestTree
test =
  Tasty.testPropertyNamed "parse" "test_parse_pattern" $ Hedgehog.property $ do
    pattern <- Hedgehog.forAll patternGen
    Megaparsec.parse Pattern.parser "" (Pattern.asText pattern)
      === Right pattern

test :: TestTree
test =
  Tasty.testGroup
    "Static patterns"
    [ testFileMatchesAndToPath
        "single file"
        (fromList [fromList [Pattern.FrText "some-path"]])
        (trackWithFile [absfile|/some-path.mp3|]),
      testFileMatchesAndToPath
        "single file in fragments"
        ( fromList
            [ fromList
                [ Pattern.FrText "some",
                  Pattern.FrText "-splitted",
                  Pattern.FrText "-path"
                ]
            ]
        )
        (trackWithFile [absfile|/some-splitted-path.mp3|]),
      testFileMatchesAndToPath
        "file in a directory"
        ( fromList
            [ fromList [Pattern.FrText "some-path"],
              fromList [Pattern.FrText "to-somewhere"]
            ]
        )
        (trackWithFile [absfile|/some-path/to-somewhere.mp3|]),
      testFileMatchesAndToPath
        "file in a directory with an extension"
        ( fromList
            [ fromList [Pattern.FrText "some-path"],
              fromList [Pattern.FrText "to-somewhere"],
              fromList [Pattern.FrText "audio"]
            ]
        )
        (trackWithFile [absfile|/some-path/to-somewhere/audio.mp3|])
    ]

test :: TestTree
test =
  Tasty.testGroup
    "Tag patterns"
    [ testFileMatchesAndToPath
        "one fragment per component"
        ( fromList
            [ fromList [Pattern.FrPlaceholder $ Pattern.PlTag Tag.Genre],
              fromList [Pattern.FrPlaceholder $ Pattern.PlTag Tag.Artist],
              fromList [Pattern.FrPlaceholder $ Pattern.PlTag Tag.Album],
              fromList [Pattern.FrPlaceholder $ Pattern.PlTag Tag.Disc],
              fromList [Pattern.FrPlaceholder $ Pattern.PlTag Tag.Title]
            ]
        )
        $ trackWithFile
          [absfile|/genre/artist/album/1/title.mp3|],
      testFileMatchesAndToPath
        "multiple fragments per component"
        ( fromList
            [ fromList [Pattern.FrPlaceholder $ Pattern.PlTag Tag.Genre],
              fromList [Pattern.FrPlaceholder $ Pattern.PlTag Tag.Artist],
              fromList
                [ Pattern.FrPlaceholder $ Pattern.PlTag Tag.Year,
                  Pattern.FrText "-",
                  Pattern.FrPlaceholder $ Pattern.PlTag Tag.Disc,
                  Pattern.FrText "-",
                  Pattern.FrPlaceholder $ Pattern.PlTag Tag.Album
                ],
              fromList
                [ Pattern.FrPlaceholder $ Pattern.PlTag Tag.Track,
                  Pattern.FrText "-",
                  Pattern.FrPlaceholder $ Pattern.PlTag Tag.Title
                ]
            ]
        )
        $ trackWithFile
          [absfile|/genre/artist/2024-1-album/1-title.mp3|],
      testFileMatchesAndToPath
        "albumartist fallback to artist when albumartist is not present"
        ( fromList
            [ fromList [Pattern.FrPlaceholder $ Pattern.PlTag Tag.Genre],
              fromList
                [ Pattern.FrPlaceholder $
                    Pattern.PlTagOr Tag.AlbumArtist Tag.Artist
                ],
              fromList [Pattern.FrPlaceholder $ Pattern.PlTag Tag.Album],
              fromList [Pattern.FrPlaceholder $ Pattern.PlTag Tag.Title]
            ]
        )
        ( (trackWithFile [absfile|/genre/artist/album/title.mp3|])
            { AudioTrack.atAlbumArtist = ""
            }
        ),
      testFileMatchesAndToPath
        "albumartist use albumartist when it is present"
        ( fromList
            [ fromList [Pattern.FrPlaceholder $ Pattern.PlTag Tag.Genre],
              fromList
                [ Pattern.FrPlaceholder $
                    Pattern.PlTagOr Tag.AlbumArtist Tag.Artist
                ],
              fromList [Pattern.FrPlaceholder $ Pattern.PlTag Tag.Album],
              fromList [Pattern.FrPlaceholder $ Pattern.PlTag Tag.Title]
            ]
        )
        (trackWithFile [absfile|/genre/albumartist/album/title.mp3|]),
      testFileMatchesAndToPath
        "same with optional if albumartist is not present"
        ( fromList
            [ fromList [Pattern.FrPlaceholder $ Pattern.PlTag Tag.Genre],
              fromList
                [ Pattern.FrPlaceholder $
                    Pattern.PlOptional "before-" Tag.AlbumArtist "-after"
                ],
              fromList [Pattern.FrPlaceholder $ Pattern.PlTag Tag.Album],
              fromList [Pattern.FrPlaceholder $ Pattern.PlTag Tag.Title]
            ]
        )
        ( (trackWithFile [absfile|/genre/album/title.mp3|])
            { AudioTrack.atAlbumArtist = ""
            }
        ),
      testFileMatchesAndToPath
        "same with optional if albumartist is present"
        ( fromList
            [ fromList [Pattern.FrPlaceholder $ Pattern.PlTag Tag.Genre],
              fromList
                [ Pattern.FrPlaceholder $
                    Pattern.PlOptional "before-" Tag.AlbumArtist "-after"
                ],
              fromList [Pattern.FrPlaceholder $ Pattern.PlTag Tag.Album],
              fromList [Pattern.FrPlaceholder $ Pattern.PlTag Tag.Title]
            ]
        )
        (trackWithFile [absfile|/genre/before-albumartist-after/album/title.mp3|])
    ]

testFileMatchesAndToPath ::
  Tasty.TestName ->
  Pattern.Pattern ->
  AudioTrack.AudioTrack ->
  Tasty.TestTree
testFileMatchesAndToPath text pattern track@AudioTrack.AudioTrack {..} =
  Tasty.testGroup
    text
    [ Tasty.testCase "FileMatches" $
        filenameMatchesNoFormatting
          pattern
          track
          `shouldBe` True,
      Tasty.testCase
        "toPath"
        $ ((root </>) <$> Pattern.toPath Pattern.noFormatting track pattern)
          `shouldBe` Just atFile
    ]
  where
    root = [absdir|/|]

test :: TestTree
test =
  Tasty.testGroup
    "Other patterns"
    [ Tasty.testCase
        "fail if the filename is smaller than the pattern"
        $ ( filenameMatchesNoFormatting $
              fromList
                [ fromList [Pattern.FrText "some-path"],
                  fromList [Pattern.FrPlaceholder $ Pattern.PlTag Tag.Title]
                ]
          )
          (trackWithFile [absfile|/1-title.mp3|])
          `shouldBe` False
    ]

test :: TestTree
test =
  Tasty.testGroup
    "Pattern parser"
    [ Tasty.testCase "single text" $
        Megaparsec.parse Pattern.parser "" "sometext"
          `shouldParse` fromList
            [fromList [Pattern.FrText "sometext"]],
      Tasty.testCase "mixed text" $
        Megaparsec.parse Pattern.parser "" "sometext{artist}moretext"
          `shouldParse` fromList
            [ fromList
                [ Pattern.FrText "sometext",
                  Pattern.FrPlaceholder $ Pattern.PlTag Tag.Artist,
                  Pattern.FrText "moretext"
                ]
            ],
      Tasty.testCase "some text" $
        Megaparsec.parse Pattern.parser "" "sometext/with/slash"
          `shouldParse` fromList
            [ fromList [Pattern.FrText "sometext"],
              fromList [Pattern.FrText "with"],
              fromList [Pattern.FrText "slash"]
            ],
      Tasty.testCase "some mixed text" $
        Megaparsec.parse
          Pattern.parser
          ""
          "prefix-{artist}/{album}-suffix/before-{title}-after"
          `shouldParse` fromList
            [ fromList
                [ Pattern.FrText "prefix-",
                  Pattern.FrPlaceholder $ Pattern.PlTag Tag.Artist
                ],
              fromList
                [ Pattern.FrPlaceholder $ Pattern.PlTag Tag.Album,
                  Pattern.FrText "-suffix"
                ],
              fromList
                [ Pattern.FrText "before-",
                  Pattern.FrPlaceholder $ Pattern.PlTag Tag.Title,
                  Pattern.FrText "-after"
                ]
            ]
    ]

test :: TestTree
test =
  Tasty.testGroup
    "Formatting options"
    [ Tasty.testCase "no substitutions" $
        filenameMatches
          trackDashTitle
          Pattern.noFormatting
          (trackWithTitleAndFile "title" [absfile|/1-title.mp3|])
          `shouldBe` True,
      Tasty.testGroup
        "unwanted char"
        [ Tasty.testCase "remove slashes" $
            filenameMatches
              trackDashTitle
              Pattern.noFormatting
              ( trackWithTitleAndFile
                  "title/with/slashes"
                  [absfile|/1-titlewithslashes.mp3|]
              )
              `shouldBe` True,
          Tasty.testCase "slashes to underscore" $
            filenameMatches
              trackDashTitle
              ( Pattern.noFormatting
                  { Pattern.foCharActions = [('/', Pattern.ChReplace '_')]
                  }
              )
              ( trackWithTitleAndFile
                  "title/with/slashes"
                  [absfile|/1-title_with_slashes.mp3|]
              )
              `shouldBe` True,
          Tasty.testCase "remove slashes, to underscore colons" $
            filenameMatches
              trackDashTitle
              ( Pattern.noFormatting
                  { Pattern.foCharActions =
                      [ ('/', Pattern.ChRemove),
                        (':', Pattern.ChReplace '_')
                      ]
                  }
              )
              ( trackWithTitleAndFile
                  "title:with:colons/and/slash"
                  [absfile|/1-title_with_colonsandslash.mp3|]
              )
              `shouldBe` True
        ],
      Tasty.testGroup
        "tag with spaces"
        [ Tasty.testCase "keep" $
            filenameMatches
              trackDashTitle
              Pattern.noFormatting
              ( trackWithTitleAndFile
                  "title with spaces"
                  [absfile|/1-title with spaces.mp3|]
              )
              `shouldBe` True,
          Tasty.testCase "to underscore" $
            filenameMatches
              trackDashTitle
              ( Pattern.noFormatting
                  { Pattern.foCharActions = [(' ', Pattern.ChReplace '_')]
                  }
              )
              ( trackWithTitleAndFile
                  "title with spaces"
                  [absfile|/1-title_with_spaces.mp3|]
              )
              `shouldBe` True
        ],
      Tasty.testCase "zero padding" $
        filenameMatches
          trackDashTitle
          (Pattern.noFormatting {Pattern.foPadTrackNumbers = Pattern.Pad 3})
          (trackWithTitleAndFile "title" [absfile|/001-title.mp3|])
          `shouldBe` True,
      Tasty.testCase "ignore padding" $
        filenameMatches
          trackDashTitle
          (Pattern.noFormatting {Pattern.foPadTrackNumbers = Pattern.Ignore})
          (trackWithTitleAndFile "title" [absfile|/001-title.mp3|])
          `shouldBe` True,
      Tasty.testCase "max placeholder length" $
        filenameMatches
          trackDashTitle
          (Pattern.noFormatting {Pattern.foPlaceholderMaxLength = 5})
          (trackWithTitleAndFile "123456789" [absfile|/1-12345.mp3|])
          `shouldBe` True
    ]
  where
    trackDashTitle =
      fromList
        [ fromList
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
  isRight . Track.check (Track.FilenameMatches pattern formatting)

trackWithTitleAndFile ::
  Text -> Path.Path Path.Abs Path.File -> AudioTrack.AudioTrack
trackWithTitleAndFile title file =
  AudioTrack.AudioTrack
    { atFile = file,
      atTitle = HTagLib.mkTitle title,
      atArtist = HTagLib.mkArtist "artist",
      atAlbum = HTagLib.mkAlbum "album",
      atAlbumArtist = HTagLib.mkAlbumArtist "albumartist",
      atDisc = HTagLib.mkDiscNumber 1,
      atGenre = HTagLib.mkGenre "genre",
      atYear = HTagLib.mkYear 2024,
      atTrack = HTagLib.mkTrackNumber 1
    }

trackWithFile :: Path.Path Path.Abs Path.File -> AudioTrack.AudioTrack
trackWithFile = trackWithTitleAndFile "title"
