{- AUTOCOLLECT.TEST -}

module Tests.Example
  (
  {- AUTOCOLLECT.TEST.export -}
  )
where

import Test.Hspec.Expectations (shouldBe)
import Test.Tasty (testGroup)
import Test.Tasty.HUnit (testCase)

test :: TestTree
test =
  testGroup
    "Example"
    [ testCase "example test" $
        1 + 1 `shouldBe` (2 :: Int)
    ]
