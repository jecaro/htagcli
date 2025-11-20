{- AUTOCOLLECT.TEST -}

module Tests.AudioTrack
  (
  {- AUTOCOLLECT.TEST.export -}
  )
where

import AudioTrack qualified
import Data.Text qualified as Text
import Hedgehog ((===))
import Hedgehog qualified
import Hedgehog.Gen qualified as HedgehogGen
import Hedgehog.Range qualified as HedgehogRange
import Path qualified
import Sound.HTagLib qualified as HTagLib
import Sound.HTagLib.Extra qualified as HTagLib
import Test.Tasty qualified as Tasty
import Test.Tasty.Hedgehog qualified as Tasty
import Text.Megaparsec qualified as Megaparsec

audioTrackGen :: Hedgehog.Gen AudioTrack.AudioTrack
audioTrackGen = do
  atFile <- absFileGen
  atTitle <- HTagLib.mkTitle <$> textGen
  atArtist <- HTagLib.mkArtist <$> textGen
  atAlbumArtist <- HTagLib.mkAlbumArtist <$> textGen
  atAlbum <- HTagLib.mkAlbum <$> textGen
  atGenre <- HTagLib.mkGenre <$> textGen
  atYear <- choiceMaybeInt HTagLib.mkYear 1900 2100
  atTrack <- choiceMaybeInt HTagLib.mkTrackNumber 1 10
  atDisc <- choiceMaybeInt HTagLib.mkDiscNumber 1 5
  pure AudioTrack.AudioTrack {..}
  where
    textGen = HedgehogGen.text (HedgehogRange.linear 0 100) HedgehogGen.alphaNum
    choiceMaybeInt mk from to =
      HedgehogGen.choice
        [ pure Nothing,
          mk <$> HedgehogGen.int (HedgehogRange.linear from to)
        ]

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
