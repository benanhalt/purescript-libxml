module Test.Main where


import Prelude

import Effect (Effect)
import Test.Document (documentTest)
import Test.Element (elementTest)
import Test.Unit (suite)
import Test.Unit.Main (runTest)


main :: Effect Unit
main = runTest do
  suite "document tests" documentTest
  suite "element tests" elementTest
