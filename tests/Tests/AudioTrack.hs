{- AUTOCOLLECT.TEST -}
{-# LANGUAGE QuasiQuotes #-}

module Tests.AudioTrack
  (
  {- AUTOCOLLECT.TEST.export -}
  )
where

import AudioTrack qualified
import Control.Monad.Morph qualified as Morph
import Control.Monad.Trans.Resource qualified as Resource
import Data.Char qualified as Char
import Data.Text qualified as Text
import Hedgehog ((===))
import Hedgehog qualified
import Hedgehog.Gen qualified as HedgehogGen
import Hedgehog.Range qualified as HedgehogRange
import Path (relfile, (</>))
import Path qualified
import Path.IO qualified as Path
import Sound.HTagLib qualified as HTagLib
import Sound.HTagLib.Extra qualified as HTagLib
import Test.Tasty qualified as Tasty
import Test.Tasty.Hedgehog qualified as Tasty
import Text.Megaparsec qualified as Megaparsec

audioTrackGen :: Hedgehog.Gen AudioTrack.AudioTrack
audioTrackGen = do
  atFile <- absFileGen
  atTitle <- HTagLib.mkTitle <$> alphaNumGen
  atArtist <- HTagLib.mkArtist <$> alphaNumGen
  atAlbumArtist <- HTagLib.mkAlbumArtist <$> alphaNumGen
  atAlbum <- HTagLib.mkAlbum <$> alphaNumGen
  atGenre <- HTagLib.mkGenre . sanitizeGenre <$> alphaNumGen
  atYear <- choiceMaybeInt HTagLib.mkYear 1900 2100
  atTrack <- choiceMaybeInt HTagLib.mkTrackNumber 1 10
  atDisc <- choiceMaybeInt HTagLib.mkDiscNumber 1 5
  pure AudioTrack.AudioTrack {..}
  where
    alphaNumGen =
      HedgehogGen.text (HedgehogRange.linear 0 100) HedgehogGen.alphaNum
    choiceMaybeInt mk from to =
      HedgehogGen.choice
        [ pure Nothing,
          mk <$> HedgehogGen.int (HedgehogRange.linear from to)
        ]
    -- HTagLib doesn't like genre made of short all-digit strings
    sanitizeGenre text
      | Text.length text <= 3 && Text.all Char.isDigit text =
          Text.dropWhile Char.isDigit text
      | otherwise = text

absFileGen :: Hedgehog.Gen (Path.Path Path.Abs Path.File)
absFileGen = do
  pieces <- HedgehogGen.list (HedgehogRange.linear 1 10) pathPieceGen
  let filepath = fold $ "/" : intersperse "/" pieces
  HedgehogGen.just $ pure $ Path.parseAbsFile filepath
  where
    pathPieceGen =
      HedgehogGen.string (HedgehogRange.linear 1 20) HedgehogGen.alphaNum

test :: TestTree
test =
  Tasty.testGroup
    "Parse"
    [ Tasty.testPropertyNamed "one" "test_parse_one" $ Hedgehog.property $ do
        audioTrack <- Hedgehog.forAll audioTrackGen
        Megaparsec.parse AudioTrack.parser "" (AudioTrack.asText audioTrack)
          === Right audioTrack,
      Tasty.testPropertyNamed "many" "test_parse_many" $ Hedgehog.property $ do
        audioTracks <-
          Hedgehog.forAll $
            HedgehogGen.list (HedgehogRange.linear 0 50) audioTrackGen
        let text = Text.intercalate "\n" (AudioTrack.asText <$> audioTracks)
        Megaparsec.parse AudioTrack.audioTracksP "" text
          === Right audioTracks
    ]

test :: TestTree
test =
  Tasty.testPropertyNamed "roundtrip" "test_set_get" $ do
    Hedgehog.property . Morph.hoist Resource.runResourceT $ do
      (_, tempDir) <-
        Resource.allocate
          (flip Path.createTempDir "htagcli" =<< Path.getTempDir)
          Path.removeDirRecur

      let dstAbsFile = tempDir </> [Path.relfile|track.mp3|]
      Path.copyFile [relfile|./data/sample.mp3|] dstAbsFile

      audioTrackWithBadFile <- Hedgehog.forAll audioTrackGen
      let audioTrackIn = audioTrackWithBadFile {AudioTrack.atFile = dstAbsFile}
      AudioTrack.setTags audioTrackIn

      audioTrackOut <- AudioTrack.getTags dstAbsFile
      audioTrackOut === audioTrackIn
