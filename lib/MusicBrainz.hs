{-# LANGUAGE QuasiQuotes #-}

module MusicBrainz (search, searchAlbum) where

import Control.Concurrent qualified as Concurrent
import Data.String.Interpolate (i, __i)
import Data.Text qualified as Text
import Model.Album qualified as Album
import Model.AudioTrack qualified as AudioTrack
import Model.Disc qualified as Disc
import MusicBrainz.Average qualified as Average
import MusicBrainz.Req qualified as Req
import MusicBrainz.Similarity qualified as Similarity
import MusicBrainz.Types qualified as MusicBrainz
import Sound.HTagLib qualified as HTagLib
import Sound.HTagLib.Extra qualified as HTagLib

searchAlbum :: Int -> Album.Album -> IO ()
searchAlbum maxResults album =
  search maxResults (Album.albumArtist album) (Album.album album) (Just album)

search ::
  Int -> HTagLib.AlbumArtist -> HTagLib.Album -> Maybe Album.Album -> IO ()
search maxResults albumArtist album mbLocalAlbum = do
  putTextLn [i|Searching: "#{albumArtistText}" - "#{albumText}"|]
  putTextLn ""

  releases <- Req.searchReleases maxResults albumArtist album
  let nbReleases = length releases
      s :: Text
      s = if nbReleases > 1 then "s" else ""
  putTextLn [i|#{nbReleases} release#{s} found\n|]

  forM_ (zip [1 ..] releases) $ \(idx, release) -> do
    -- Official MusicBrainz API rate limit
    Concurrent.threadDelay 1_000_000
    detail <- Req.lookupRelease $ MusicBrainz.reId release
    displayRelease idx detail mbLocalAlbum
  where
    albumArtistText = HTagLib.unAlbumArtist albumArtist
    albumText = HTagLib.unAlbum album

displayRelease ::
  Int ->
  MusicBrainz.ReleaseDetail ->
  Maybe Album.Album ->
  IO ()
displayRelease idx detail@(MusicBrainz.ReleaseDetail {..}) mbAlbum = do
  putTextLn
    [__i|
      #{idx}. ID: #{rdId} #{overallSuffix}
         Artist: #{artist} #{artistSuffix}
         Album: #{rdTitle} #{titleSuffix}
         Year: #{year} #{yearSuffix}
         Discs: #{mediaCount} #{mediaCountSuffix}
         Tracks: #{trackCount} #{trackCountSuffix}
      |]
  putTextLn ""

  traverse_ (uncurry displayMedia) mediaAndDiscs
  where
    medias = toList rdMedia
    discs = orMempty (fmap Just . toList . Album.discs) mbAlbum
    mediaAndDiscs = zip medias (discs <> repeat Nothing)

    overallSuffix =
      inParensMaybe (percentage . Similarity.detailAndAlbum detail) mbAlbum

    artist = MusicBrainz.artistCreditToText rdArtistCredit
    localArtist = HTagLib.unAlbumArtist . Album.albumArtist <$> mbAlbum
    artistSuffix = orMempty (similaritySuffix artist) localArtist

    localTitle = HTagLib.unAlbum . Album.album <$> mbAlbum
    titleSuffix = orMempty (similaritySuffix rdTitle) localTitle

    year :: Text
    year = maybe "unknown" show rdDate
    localYears = orMempty (fmap HTagLib.unYear . Album.years) mbAlbum
    yearSuffix
      | null localYears || localYears == maybeToList rdDate = ""
      | otherwise = inParens $ Text.intercalate ", " $ show <$> localYears

    localDiscCount = length . Album.discs <$> mbAlbum
    mediaCount = length medias
    mediaCountSuffix = orMempty (showIfDifferent mediaCount) localDiscCount

    trackCount =
      sum $ length . MusicBrainz.meTracks <$> medias
    localTrackCount = length . (Disc.tracks <=< Album.discs) <$> mbAlbum
    trackCountSuffix = orMempty (showIfDifferent trackCount) localTrackCount

displayMedia :: MusicBrainz.Media -> Maybe Disc.Disc -> IO ()
displayMedia media@MusicBrainz.Media {..} mDisc = do
  putTextLn [i|   Disc #{mePosition}: #{discSimilarity} - Tracks: #{trackCount} #{trackCountSuffix}|]
  putTextLn ""

  forM_ tracksAndLocalTracks $ \(MusicBrainz.Track {..}, mLocalTitle) -> do
    let trackSuffix =
          orMempty
            (similaritySuffix (MusicBrainz.rcTitle trRecording))
            mLocalTitle
        title = MusicBrainz.rcTitle trRecording
    putTextLn [i|     #{trPosition}. #{title} #{trackSuffix}|]

  putTextLn ""
  where
    tracks = toList meTracks
    tracksAndLocalTracks = zip tracks $ localTitles <> repeat Nothing

    discSimilarity =
      inParensMaybe
        (percentage . Average.toDouble . Similarity.mediaAndDisc media)
        mDisc

    trackCount = length tracks
    localTrackCount = length . Disc.tracks <$> mDisc
    trackCountSuffix = orMempty (showIfDifferent trackCount) localTrackCount

    localTitles =
      orMempty
        (fmap (Just . HTagLib.unTitle . AudioTrack.atTitle) . toList . Disc.tracks)
        mDisc

percentage :: Double -> Text
percentage value = show (round (value * 100) :: Int) <> "%"

displaySimilarity :: Double -> Text -> Text
displaySimilarity value text =
  inParens $
    percentage value <> if value < 0.95 then " - " <> text else ""

inParens :: Text -> Text
inParens text = "(" <> text <> ")"

showIfDifferent :: (Show a, Eq a) => a -> a -> Text
showIfDifferent mbValue localValue
  | localValue == mbValue = ""
  | otherwise = inParens (show localValue)

similaritySuffix :: Text -> Text -> Text
similaritySuffix searchText localText =
  displaySimilarity (Similarity.text searchText localText) localText

inParensMaybe :: (a -> Text) -> Maybe a -> Text
inParensMaybe aToText mbText = orMempty (inParens . aToText) mbText

orMempty :: (Monoid m) => (a -> m) -> Maybe a -> m
orMempty = maybe mempty
