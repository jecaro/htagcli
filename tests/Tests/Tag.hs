{- AUTOCOLLECT.TEST -}

module Tests.Tag
  (
  {- AUTOCOLLECT.TEST.export -}
  )
where

import Hedgehog (property, (===))
import Hedgehog qualified
import Hedgehog.Gen qualified as Hedgehog
import Tag qualified
import Test.Tasty.Hedgehog (testPropertyNamed)
import Text.Megaparsec qualified as Megaparsec

test :: TestTree
test =
  testPropertyNamed "roundtrip" "roundtrip" $ property $ do
    tag <- Hedgehog.forAll Hedgehog.enumBounded
    Megaparsec.parse Tag.parser "" (Tag.asText tag) === Right tag
