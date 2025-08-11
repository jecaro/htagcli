module Main where

import AudioTrack (getTags, render)
import Options
import Options.Applicative (execParser)
import Path (prjSomeBase, toFilePath)
import Sound.HTagLib
  ( albumSetter,
    artistSetter,
    genreSetter,
    setTags,
    titleSetter,
    trackNumberSetter,
    yearSetter,
  )

main :: IO ()
main = do
  options <- execParser optionsInfo
  case options of
    Display DisplayOptions {..} ->
      traverse_ ((putTextLn . render) <=< getTags) doFiles
    Edit EditOptions {..} -> do
      forM_ eoFiles $ \file -> do
        let filename = prjSomeBase toFilePath file
            setter =
              fold $
                catMaybes
                  [ titleSetter <$> eoTitle,
                    artistSetter <$> eoArtist,
                    albumSetter <$> eoAlbum,
                    genreSetter <$> eoGenre,
                    toSetter yearSetter eoYear,
                    toSetter trackNumberSetter eoTrack
                  ]
        setTags filename Nothing setter
  where
    toSetter _ Nothing = Nothing
    toSetter setter (Just Remove) = Just $ setter Nothing
    toSetter setter (Just (Set v)) = Just . setter $ Just v
