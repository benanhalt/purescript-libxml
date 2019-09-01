module Test.Main where


import Prelude
import Effect (Effect)

import Test.Unit.Main (runTest)
import Test.Document (documentTest)


main :: Effect Unit
main = runTest documentTest
