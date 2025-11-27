{- AUTOCOLLECT.TEST -}
{-# LANGUAGE QuasiQuotes #-}

module Tests.Check.Artist
  (
  {- AUTOCOLLECT.TEST.export -}
  )
where

import Check.Artist qualified as Artist
import Model.Artist qualified as Artist
import Path (absdir)
import Relude.Unsafe qualified as Unsafe
import Sound.HTagLib qualified as HTagLib
import Test.Hspec.Expectations (shouldBe)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit qualified as Tasty
import Tests.Common qualified as Common

test :: TestTree
test =
  Tasty.testGroup
    "check artist"
    [ Tasty.testCase "two albums of the same artist with the same genre" $ do
        let album1 =
              tenTracksAlbum (HTagLib.mkAlbum "album-1") (HTagLib.mkGenre "Rock")
            album2 =
              tenTracksAlbum (HTagLib.mkAlbum "album-2") (HTagLib.mkGenre "Rock")
            artist =
              Unsafe.fromJust $
                Artist.mkArtist $
                  fromList [album1, album2]
        Artist.check Artist.SameGenre artist
          `shouldBe` Right (),
      Tasty.testCase "two albums of the same artist with different genres" $ do
        let rock = HTagLib.mkGenre "Rock"
            pop = HTagLib.mkGenre "Pop"
        let album1 =
              tenTracksAlbum (HTagLib.mkAlbum "album-1") pop
            album2 =
              tenTracksAlbum (HTagLib.mkAlbum "album-2") rock
            artist =
              Unsafe.fromJust $
                Artist.mkArtist $
                  fromList [album1, album2]
        Artist.check Artist.SameGenre artist
          `shouldBe` Left (Artist.SameGenreError $ fromList [pop, rock])
    ]
  where
    tenTracksAlbum = Common.tenTracksAlbum' [absdir|/path/to|]
