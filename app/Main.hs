module Main where

import AudioTrack (AudioTrack (..), getTags, haveTag)
import AudioTrack qualified
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
import Data.Text qualified as Text
import Options
  ( CheckOptions (..),
    Directory (..),
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
    unGenre,
    yearSetter,
  )
import Tag qualified

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
          .| mapM_C (putTextLn . AudioTrack.render <=< getTags)
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
    Check CheckOptions {..} -> do
      runConduitRes $
        fileOrDirectoryC coFilesOrDirectory
          .| mapM_C
            ( \file -> do
                let filename = prjSomeBase toFilePath file
                track <- getTags file
                let missingTags =
                      mapMaybe
                        ( \t ->
                            if haveTag t track
                              then Nothing
                              else Just t
                        )
                        coTags
                unless (null missingTags) $
                  putTextLn $
                    fromString filename
                      <> " missing "
                      <> Text.intercalate ", " (Tag.render <$> missingTags)
                let genreOk =
                      null coGenreAmong
                        || unGenre (atGenre track) `elem` coGenreAmong
                unless genreOk $
                  putTextLn $
                    fromString filename
                      <> " has genre "
                      <> unGenre (atGenre track)
                      <> ", expected one of "
                      <> Text.intercalate ", " coGenreAmong
            )
  where
    toSetter _ Nothing = Nothing
    toSetter setter (Just Remove) = Just $ setter Nothing
    toSetter setter (Just (Set v)) = Just . setter $ Just v
