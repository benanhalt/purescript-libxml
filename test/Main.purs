module Test.Main where

import Libxml
import Prelude

import Data.Array (length)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), isNothing)
import Data.Traversable (for_)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Test.Unit (failure, suite, test, timeout)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

import Test.Document (documentTest)


main :: Effect Unit
main = runTest documentTest
