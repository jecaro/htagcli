{- AUTOCOLLECT.TEST -}
{-# LANGUAGE QuasiQuotes #-}

module Tests.Check
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
import Test.Tasty (testGroup)
import Test.Tasty.HUnit (testCase)

test :: TestTree
test =
  testGroup
    "FilenameMatches"
    [ testCase
        "fail with MissingTags if the file doesn't contains a placeholder tag"
        $ Check.check
          ( Check.FilenameMatches $
              NonEmpty.fromList
                [NonEmpty.fromList [Pattern.Placeholder Tag.Artist]]
          )
          track
          `shouldBe` Left (Check.MissingTags (NonEmpty.fromList [Tag.Artist])),
      testCase
        "fail with FilenameMismatch if the file contains a placeholder tag"
        $ Check.check
          ( Check.FilenameMatches $
              NonEmpty.fromList
                [NonEmpty.fromList [Pattern.Placeholder Tag.Album]]
          )
          track
          `shouldBe` Left (Check.FilenameMismatch "album" "title"),
      testCase
        "succeed if the file matches the pattern"
        $ Check.check
          ( Check.FilenameMatches $
              NonEmpty.fromList
                [NonEmpty.fromList [Pattern.Placeholder Tag.Title]]
          )
          track
          `shouldBe` Right ()
    ]

track :: AudioTrack.AudioTrack
track =
  AudioTrack.AudioTrack
    { atFile = Path.Rel [relfile|./title.mp3|],
      atTitle = HTagLib.mkTitle "title",
      atArtist = HTagLib.mkArtist "",
      atAlbum = HTagLib.mkAlbum "album",
      atGenre = HTagLib.mkGenre "genre",
      atYear = HTagLib.mkYear 2024,
      atTrack = HTagLib.mkTrackNumber 1
    }
