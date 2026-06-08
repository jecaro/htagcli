module Tests.Config (test) where

import Config qualified
import Test.Hspec.Expectations (shouldSatisfy)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit qualified as Tasty
import UnliftIO.Exception qualified as Exception

test :: Tasty.TestTree
test = Tasty.testGroup "Config" [testParseDefaultConfig]

testParseDefaultConfig :: Tasty.TestTree
testParseDefaultConfig =
  Tasty.testCase "Default config should parse" $ do
    result <-
      Exception.tryAny $ Config.parseByteString Config.defaultConfigContent
    result `shouldSatisfy` isRight
