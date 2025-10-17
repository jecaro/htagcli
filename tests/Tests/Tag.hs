{- AUTOCOLLECT.TEST -}

module Tests.Tag
  (
  {- AUTOCOLLECT.TEST.export -}
  )
where

import Hedgehog ((===))
import Hedgehog qualified
import Hedgehog.Gen qualified as Hedgehog
import Tag qualified
import Test.Tasty.Hedgehog qualified as Tasty
import Text.Megaparsec qualified as Megaparsec

test :: TestTree
test =
  Tasty.testPropertyNamed "roundtrip" "roundtrip" $ Hedgehog.property $ do
    tag <- Hedgehog.forAll Hedgehog.enumBounded
    Megaparsec.parse Tag.parser "" (Tag.asText tag) === Right tag
