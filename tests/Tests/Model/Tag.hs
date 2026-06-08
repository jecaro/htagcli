module Tests.Model.Tag (test) where

import Hedgehog ((===))
import Hedgehog qualified
import Hedgehog.Gen qualified as Hedgehog
import Model.Tag qualified as Tag
import Test.Tasty qualified as Tasty
import Test.Tasty.Hedgehog qualified as Tasty
import Text.Megaparsec qualified as Megaparsec

test :: Tasty.TestTree
test = Tasty.testGroup "Model.Tag" [testParseRoundTrip]

testParseRoundTrip :: Tasty.TestTree
testParseRoundTrip =
  Tasty.testPropertyNamed "roundtrip" "test_parse_one" $ Hedgehog.property $ do
    tag <- Hedgehog.forAll Hedgehog.enumBounded
    Megaparsec.parse Tag.parser "" (Tag.asText tag) === Right tag
