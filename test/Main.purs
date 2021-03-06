module Test.Main where


import Prelude

import Effect (Effect)
import Test.Attribute (attributeTest)
import Test.Comment (commentTest)
import Test.Document (documentTest)
import Test.Element (elementTest)
import Test.NameSpace (nameSpaceTest)
import Test.NodeAttribute (nodeAttributeTest)
import Test.Searching (searchingTest)
import Test.Text (textTest)
import Test.Unit (suite)
import Test.Unit.Main (runTest)


main :: Effect Unit
main = runTest do
  suite "document tests" documentTest
  suite "element tests" elementTest
  suite "attribute tests" attributeTest
  suite "node attribute tests" nodeAttributeTest
  suite "search tests" searchingTest
  suite "comment tests" commentTest
  suite "text tests" textTest
  suite "namespace tests" nameSpaceTest
