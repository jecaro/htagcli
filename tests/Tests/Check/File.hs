{- AUTOCOLLECT.TEST -}
{-# LANGUAGE QuasiQuotes #-}

module Tests.Check.File
  (
  {- AUTOCOLLECT.TEST.export -}
  )
where

import AudioTrack qualified
import Check.File qualified as File
import Path (absfile)
import Pattern qualified
import Sound.HTagLib qualified as HTagLib
import Sound.HTagLib.Extra qualified as HTagLib
import Tag qualified
import Test.Hspec.Expectations (shouldBe)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit qualified as Tasty

test :: TestTree
test =
  Tasty.testGroup
    "FilenameMatches"
    [ Tasty.testCase
        "fail with MissingTags if the file doesn't contain a placeholder tag"
        $ File.check
          ( filenameMatchesNoFormatting $
              fromList
                [ fromList
                    [Pattern.FrPlaceholder $ Pattern.PlTag Tag.Artist]
                ]
          )
          track
          `shouldBe` Left (File.MissingTags (fromList [Tag.Artist])),
      Tasty.testCase
        "fail with FilenameMismatch if the file contains a placeholder tag"
        $ File.check
          ( filenameMatchesNoFormatting $
              fromList
                [fromList [Pattern.FrPlaceholder $ Pattern.PlTag Tag.Album]]
          )
          track
          `shouldBe` Left (File.FilenameMismatch "album"),
      Tasty.testCase
        "succeed if the file matches the pattern"
        $ File.check
          ( filenameMatchesNoFormatting $
              fromList
                [fromList [Pattern.FrPlaceholder $ Pattern.PlTag Tag.Title]]
          )
          track
          `shouldBe` Right ()
    ]

filenameMatchesNoFormatting :: Pattern.Pattern -> File.Check
filenameMatchesNoFormatting pattern =
  File.FilenameMatches pattern Pattern.noFormatting

track :: AudioTrack.AudioTrack
track =
  AudioTrack.AudioTrack
    { atFile = [absfile|/title.mp3|],
      atTitle = HTagLib.mkTitle "title",
      atArtist = HTagLib.mkArtist "",
      atAlbum = HTagLib.mkAlbum "album",
      atAlbumArtist = HTagLib.mkAlbumArtist "albumartist",
      atGenre = HTagLib.mkGenre "genre",
      atYear = HTagLib.mkYear 2024,
      atTrack = HTagLib.mkTrackNumber 1,
      atDisc = HTagLib.mkDiscNumber 1
    }
