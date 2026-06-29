module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import Tests.Check.Album qualified as Check.Album
import Tests.Check.Artist qualified as Check.Artist
import Tests.Check.Disc qualified as Check.Disc
import Tests.Check.Track qualified as Check.Track
import Tests.Commands as Commands
import Tests.Config as Config
import Tests.Model.Album qualified as Model.Album
import Tests.Model.Artist qualified as Model.Artist
import Tests.Model.AudioTrack qualified as Model.AudioTrack
import Tests.Model.Disc qualified as Model.Disc
import Tests.Model.Pattern qualified as Model.Pattern
import Tests.Model.Tag qualified as Model.Tag
import Tests.MusicBrainz qualified as MusicBrainz
import Tests.MusicBrainz.Types qualified as MusicBrainz.Types

main :: IO ()
main =
  defaultMain $
    testGroup
      "Tests"
      [ Check.Album.test,
        Check.Artist.test,
        Check.Disc.test,
        Check.Track.test,
        Commands.test,
        Config.test,
        Model.Album.test,
        Model.Artist.test,
        Model.AudioTrack.test,
        Model.Disc.test,
        Model.Pattern.test,
        Model.Tag.test,
        MusicBrainz.Types.test,
        MusicBrainz.test
      ]
