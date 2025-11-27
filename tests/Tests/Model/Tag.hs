{- AUTOCOLLECT.TEST -}

module Tests.Model.Tag
  (
  {- AUTOCOLLECT.TEST.export -}
  )
where

import Hedgehog ((===))
import Hedgehog qualified
import Hedgehog.Gen qualified as Hedgehog
import Model.Tag qualified as Tag
import Test.Tasty.Hedgehog qualified as Tasty
import Text.Megaparsec qualified as Megaparsec

test :: TestTree
test =
  Tasty.testPropertyNamed "roundtrip" "test_parse_one" $ Hedgehog.property $ do
    tag <- Hedgehog.forAll Hedgehog.enumBounded
    Megaparsec.parse Tag.parser "" (Tag.asText tag) === Right tag
