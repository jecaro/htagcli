module MusicBrainz.Similarity
  ( text,
    mediaAndDisc,
    detailAndAlbum,
  )
where

import Data.List.NonEmpty qualified as NonEmpty
import Data.Text qualified as Text
import Data.Text.Metrics qualified as Metrics
import Model.Album qualified as Album
import Model.AudioTrack qualified as AudioTrack
import Model.Disc qualified as Disc
import MusicBrainz.Average qualified as Average
import MusicBrainz.Types qualified as MusicBrainz
import Sound.HTagLib qualified as HTagLib
import Sound.HTagLib.Extra qualified as HTagLib

text :: Text -> Text -> Double
text a b =
  realToFrac $ Metrics.jaroWinkler (Text.toLower a) (Text.toLower b)

mediaAndDisc :: MusicBrainz.Media -> Disc.Disc -> Average.Average
mediaAndDisc MusicBrainz.Media {..} disc =
  Average.Average
    { avSum = sum (NonEmpty.zipWith text localTracks tracks),
      avCount = max (length localTracks) (length tracks)
    }
  where
    tracks = MusicBrainz.trTitle <$> meTracks
    localTracks =
      HTagLib.unTitle . AudioTrack.atTitle <$> Disc.tracks disc

-- | Weighted similarity between a local album and a MusicBrainz release detail.
-- Weights: artist 20%, title 20%, tracks from discs 60%.
detailAndAlbum :: MusicBrainz.ReleaseDetail -> Album.Album -> Double
detailAndAlbum detail album =
  0.2 * artist + 0.2 * title + 0.6 * medias
  where
    artist =
      text
        (HTagLib.unAlbumArtist $ Album.albumArtist album)
        (MusicBrainz.artistCreditToText $ MusicBrainz.rdArtistCredit detail)
    title =
      text
        (HTagLib.unAlbum $ Album.album album)
        (MusicBrainz.rdTitle detail)
    medias =
      Average.toDouble $
        foldMap (uncurry mediaAndDisc) $
          toList $
            NonEmpty.zip (MusicBrainz.rdMedia detail) (Album.discs album)
