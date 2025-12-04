{- AUTOCOLLECT.TEST -}
{-# LANGUAGE QuasiQuotes #-}

module Tests.Check.Track
  (
  {- AUTOCOLLECT.TEST.export -}
  )
where

import Check.Track qualified as Track
import Model.AudioTrack qualified as AudioTrack
import Model.Pattern qualified as Pattern
import Model.Tag qualified as Tag
import Path (absfile)
import Sound.HTagLib qualified as HTagLib
import Sound.HTagLib.Extra qualified as HTagLib
import Test.Hspec.Expectations (shouldBe)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit qualified as Tasty

test :: TestTree
test =
  Tasty.testGroup
    "FilenameMatches"
    [ Tasty.testCase
        "fail with MissingTags if the file doesn't contain a placeholder tag"
        $ Track.check
          ( filenameMatchesNoFormatting $
              fromList
                [ fromList
                    [Pattern.FrPlaceholder $ Pattern.PlTag Tag.Artist]
                ]
          )
          track
          `shouldBe` Left (Track.MissingTags (fromList [Tag.Artist])),
      Tasty.testCase
        "fail with FilenameMismatch if the file contains a placeholder tag"
        $ Track.check
          ( filenameMatchesNoFormatting $
              fromList
                [fromList [Pattern.FrPlaceholder $ Pattern.PlTag Tag.Album]]
          )
          track
          `shouldBe` Left (Track.FilenameMismatch "album.mp3"),
      Tasty.testCase
        "succeed if the file matches the pattern"
        $ Track.check
          ( filenameMatchesNoFormatting $
              fromList
                [fromList [Pattern.FrPlaceholder $ Pattern.PlTag Tag.Title]]
          )
          track
          `shouldBe` Right ()
    ]

filenameMatchesNoFormatting :: Pattern.Pattern -> Track.Check
filenameMatchesNoFormatting pattern =
  Track.FilenameMatches pattern Pattern.noFormatting

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
