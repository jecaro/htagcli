module Main where

import AudioTrack (getTags, render)
import Conduit
  ( ConduitT,
    MonadResource,
    MonadThrow,
    filterC,
    mapMC,
    mapM_C,
    runConduitRes,
    sourceDirectoryDeep,
    yieldMany,
    (.|),
  )
import Data.Text (isSuffixOf)
import Options
  ( Directory (..),
    DisplayOptions (..),
    EditOptions (..),
    Files (..),
    FilesOrDirectory (..),
    Options (..),
    SetOrRemove (..),
    optionsInfo,
  )
import Options.Applicative (execParser)
import Path (File, SomeBase, parseSomeFile, prjSomeBase, toFilePath)
import Sound.HTagLib
  ( albumSetter,
    artistSetter,
    genreSetter,
    setTags,
    titleSetter,
    trackNumberSetter,
    yearSetter,
  )

fileOrDirectoryC ::
  (MonadResource m, MonadThrow m) =>
  FilesOrDirectory ->
  ConduitT i (SomeBase File) m ()
fileOrDirectoryC (FDFiles (Files {..})) = yieldMany fiFiles
fileOrDirectoryC (FDDirectory (Directory {..})) =
  sourceDirectoryDeep False (prjSomeBase toFilePath diPath)
    .| filterC (\filename -> any (`isSuffixOf` fromString filename) diExtensions)
    .| mapMC parseSomeFile

main :: IO ()
main = do
  options <- execParser optionsInfo
  case options of
    Display DisplayOptions {..} -> do
      runConduitRes $
        fileOrDirectoryC doFilesOrDirectory
          .| mapM_C (putTextLn . render <=< getTags)
    Edit EditOptions {..} -> do
      runConduitRes $
        fileOrDirectoryC eoFilesOrDirectory
          .| mapM_C
            ( \file -> do
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
            )
  where
    toSetter _ Nothing = Nothing
    toSetter setter (Just Remove) = Just $ setter Nothing
    toSetter setter (Just (Set v)) = Just . setter $ Just v
