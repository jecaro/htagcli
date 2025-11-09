{- AUTOCOLLECT.TEST -}
module Tests.Config
  (
  {- AUTOCOLLECT.TEST.export -}
  )
where

import Config qualified
import Test.Hspec.Expectations (shouldSatisfy)
import Test.Tasty.HUnit qualified as Tasty
import UnliftIO.Exception qualified as Exception

test :: TestTree
test =
  Tasty.testCase "Default config should parse" $ do
    result <-
      Exception.tryAny $ Config.parseByteString Config.defaultConfigContent
    result `shouldSatisfy` isRight
