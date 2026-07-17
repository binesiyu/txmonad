module Main (main) where

import Test.Hspec
import qualified StackSetSpec
import qualified LayoutSpec

main :: IO ()
main = hspec $ do
  StackSetSpec.spec
  LayoutSpec.spec
