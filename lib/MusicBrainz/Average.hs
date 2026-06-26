module MusicBrainz.Average (Average (..), toDouble) where

data Average = Average
  { avSum :: Double,
    avCount :: Int
  }

instance Semigroup Average where
  Average s1 n1 <> Average s2 n2 = Average (s1 + s2) (n1 + n2)

instance Monoid Average where
  mempty = Average 0 0

toDouble :: Average -> Double
toDouble Average {..}
  | avCount == 0 = 1
  | otherwise = avSum / fromIntegral avCount
