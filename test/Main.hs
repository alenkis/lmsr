module Main (main) where

import Hedgehog (Property, assert, property)
import MarketMaker (version)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.Hedgehog (testProperty)

main :: IO ()
main =
  defaultMain $
    testGroup
      "hs-market-maker"
      [testProperty "version is non-empty" prop_versionNonEmpty]

prop_versionNonEmpty :: Property
prop_versionNonEmpty = property $ assert (not (null version))
