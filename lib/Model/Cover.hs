module Model.Cover
  ( Cover (..),
    Size (..),
    sizeToText,
    pictureSize,
    withinRange,
  )
where

import Codec.Picture qualified as Picture
import Path qualified

data Cover = Cover
  { coPaths :: NonEmpty (Path.Path Path.Rel Path.File),
    coMinSize :: Maybe Size,
    coMaxSize :: Maybe Size
  }
  deriving (Eq, Show)

data Size = Size {siWidth :: Int, siHeight :: Int}
  deriving (Eq, Show)

sizeToText :: Size -> Text
sizeToText (Size width height) = show width <> "x" <> show height

greaterThan :: Size -> Size -> Bool
greaterThan (Size width1 height1) (Size width2 height2) =
  width1 >= width2 && height1 >= height2

lowerThan :: Size -> Size -> Bool
lowerThan (Size width1 height1) (Size width2 height2) =
  width1 <= width2 && height1 <= height2

pictureSize :: Picture.DynamicImage -> Size
pictureSize picture =
  Size
    { siWidth = Picture.dynamicMap Picture.imageWidth picture,
      siHeight = Picture.dynamicMap Picture.imageHeight picture
    }

withinRange :: Cover -> Size -> Bool
withinRange Cover {..} size =
  maybe True (size `greaterThan`) coMinSize
    && maybe True (size `lowerThan`) coMaxSize
