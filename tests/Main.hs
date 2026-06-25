module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import Tests.Check.Album qualified as Check.Album
import Tests.Check.Artist qualified as Check.Artist
import Tests.Check.Disc qualified as Check.Disc
import Tests.Check.Track qualified as Check.Track
import Tests.Commands as Commands
import Tests.Config as Config
import Tests.Model.AudioTrack qualified as Model.AudioTrack
import Tests.Model.Pattern qualified as Model.Pattern
import Tests.Model.Tag qualified as Model.Tag

main :: IO ()
main =
  defaultMain $
    testGroup
      "Tests"
      [ Commands.test,
        Config.test,
        Model.AudioTrack.test,
        Model.Pattern.test,
        Model.Tag.test,
        Check.Disc.test,
        Check.Album.test,
        Check.Artist.test,
        Check.Track.test
      ]
