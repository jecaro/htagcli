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
        let album1 = tenTracksAlbum (HTagLib.mkAlbum "album-1") rock
            album2 = tenTracksAlbum (HTagLib.mkAlbum "album-2") rock
            artist =
              Unsafe.fromJust $
                Artist.mkArtist $
                  fromList [album1, album2]
        Artist.check (Artist.SameGenre mempty) artist
          `shouldBe` Right (),
      Tasty.testCase "two albums of the same artist with different genres" $ do
        let album1 = tenTracksAlbum (HTagLib.mkAlbum "album-1") pop
            album2 = tenTracksAlbum (HTagLib.mkAlbum "album-2") rock
            artist =
              Unsafe.fromJust $
                Artist.mkArtist $
                  fromList [album1, album2]
        Artist.check (Artist.SameGenre mempty) artist
          `shouldBe` Left (Artist.SameGenreError genres),
      Tasty.testCase
        "two albums of the same artist with different genres but \
        \with exceptions"
        $ do
          let album1 = tenTracksAlbum (HTagLib.mkAlbum "album-1") pop
              album2 = tenTracksAlbum (HTagLib.mkAlbum "album-2") rock
              artist =
                Unsafe.fromJust $
                  Artist.mkArtist $
                    fromList [album1, album2]
          Artist.check
            (Artist.SameGenre (fromList [("Album Artist", genres)]))
            artist
            `shouldBe` Right ()
    ]
  where
    tenTracksAlbum = Common.tenTracksAlbum' [absdir|/path/to|]
    rock = HTagLib.mkGenre "Rock"
    pop = HTagLib.mkGenre "Pop"
    genres = fromList [pop, rock]
