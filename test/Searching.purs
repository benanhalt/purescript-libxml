module Test.Searching (searchingTest) where

import Libxml.Types
import Prelude

import Data.Array (length)
import Data.Maybe (Maybe(..), fromJust)
import Effect.Class (liftEffect)
import Libxml.Document (defaultDocEncodingAndVersion, docCreateRoot, docFind, docGetElement, newDoc)
import Libxml.Element (elementAddChild, elementAddNode, elementAttr, elementAttrs, elementGetElement, elementSetAttr)
import Libxml.Node (nextSibling, nodeIs, nodeParent, prevSibling)
import Partial.Unsafe (unsafePartial)
import Test.Unit (TestSuite, failure, test)
import Test.Unit.Assert as Assert


searchingTest :: TestSuite
searchingTest = do
  test "get" do
    doc <- liftEffect $ newDoc defaultDocEncodingAndVersion
    root <- liftEffect $ docCreateRoot "root" "" doc
    child <- liftEffect $ elementAddNode "child" "" root
    grandchild <- liftEffect $ elementAddNode "grandchild" "" child

    Assert.assert "search for child should retrieve child" =<< liftEffect do
      child' <- unsafePartial fromJust <$> docGetElement "child" doc
      nodeIs child' child

    Assert.assert "search for grandchild from child should work" =<< liftEffect do
      child' <- unsafePartial fromJust <$> docGetElement "child" doc
      grandchild' <- unsafePartial fromJust <$> elementGetElement "grandchild" child'
      nodeIs grandchild' grandchild

  test "find missing" do
    doc <- liftEffect do
      doc <- newDoc defaultDocEncodingAndVersion
      root <- docCreateRoot "root" "" doc
      pure doc

    nodes <- liftEffect $ unsafePartial do
      (Just (NodeSet nodes)) <- docFind "missing/text()" doc
      pure $ nodes

    Assert.equal 0 $ length nodes
