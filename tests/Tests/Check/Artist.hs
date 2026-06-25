{-# LANGUAGE QuasiQuotes #-}

module Tests.Check.Artist (test) where

import Check.Artist qualified as Artist
import Model.Artist qualified as Artist
import Path (absdir)
import Relude.Unsafe qualified as Unsafe
import Sound.HTagLib qualified as HTagLib
import Test.Hspec.Expectations (shouldBe)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit qualified as Tasty
import Tests.Common qualified as Common

test :: Tasty.TestTree
test =
  Tasty.testGroup
    "Check.Artist"
    [ Tasty.testCase "two discs of the same artist with the same genre" $ do
        let disc1 = tenTracksDisc (HTagLib.mkAlbum "album-1") rock
            disc2 = tenTracksDisc (HTagLib.mkAlbum "album-2") rock
            artist =
              Unsafe.fromJust $
                Artist.mkArtist $
                  fromList [disc1, disc2]
        Artist.check (Artist.SameGenre mempty) artist
          `shouldBe` Right (),
      Tasty.testCase "two discs of the same artist with different genres" $ do
        let disc1 = tenTracksDisc (HTagLib.mkAlbum "album-1") pop
            disc2 = tenTracksDisc (HTagLib.mkAlbum "album-2") rock
            artist =
              Unsafe.fromJust $
                Artist.mkArtist $
                  fromList [disc1, disc2]
        Artist.check (Artist.SameGenre mempty) artist
          `shouldBe` Left (Artist.SameGenreError genres),
      Tasty.testCase
        "two discs of the same artist with different genres but \
        \with exceptions"
        $ do
          let disc1 = tenTracksDisc (HTagLib.mkAlbum "album-1") pop
              disc2 = tenTracksDisc (HTagLib.mkAlbum "album-2") rock
              artist =
                Unsafe.fromJust $
                  Artist.mkArtist $
                    fromList [disc1, disc2]
          Artist.check
            (Artist.SameGenre (fromList [("Album Artist", genres)]))
            artist
            `shouldBe` Right ()
    ]
  where
    tenTracksDisc = Common.tenTracksDisc' [absdir|/path/to|]
    rock = HTagLib.mkGenre "Rock"
    pop = HTagLib.mkGenre "Pop"
    genres = fromList [pop, rock]
