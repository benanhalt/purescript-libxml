module Test.Comment (commentTest) where

import Libxml.Types
import Prelude

import Effect.Class (liftEffect)
import Libxml.Comment (commentSetText, commentText, newComment)
import Libxml.Document (defaultDocEncodingAndVersion, newDoc)
import Libxml.Node (nodeToString)
import Test.Unit (TestSuite, test)
import Test.Unit.Assert as Assert


commentTest :: TestSuite
commentTest = do
  test "new comment" do
    doc <- liftEffect $ newDoc defaultDocEncodingAndVersion
    comm <- liftEffect $ newComment "comment1" doc
    Assert.equal "comment1" =<< (liftEffect $ commentText comm)

  test "text" do
    doc <- liftEffect $ newDoc defaultDocEncodingAndVersion
    comm <- liftEffect $ newComment "" doc
    liftEffect $ commentSetText "comment2" comm
    Assert.equal "comment2" =<< (liftEffect $ commentText comm)

  test "text with special characters" do
    doc <- liftEffect $ newDoc defaultDocEncodingAndVersion
    comm <- liftEffect $ newComment "" doc
    let theText = "my comment <has> special ch&r&cters"
    liftEffect $ commentSetText theText comm
    Assert.equal theText =<< (liftEffect $ commentText comm)

  test "to string with special characters" do
    doc <- liftEffect $ newDoc defaultDocEncodingAndVersion
    comm <- liftEffect $ newComment "" doc
    let theText = "my comment <has> special ch&r&cters"
    liftEffect $ commentSetText theText comm
    Assert.equal ("<!--" <> theText <> "-->") =<< (liftEffect $ nodeToString comm)

