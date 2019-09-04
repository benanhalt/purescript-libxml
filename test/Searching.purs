module Test.Searching (searchingTest) where

import Libxml.Types
import Prelude

import Data.Array (length, zipWith)
import Data.Maybe (fromJust, isNothing)
import Data.Traversable (for_, sequence)
import Effect.Class (liftEffect)
import Libxml.Attribute (attrValue)
import Libxml.Document (defaultDocEncodingAndVersion, docCreateRoot, docFind, docGetElement, newDoc)
import Libxml.Element (elementAddChild, elementAddNode, elementAttr, elementAttrs, elementGetElement, elementSetAttr)
import Libxml.Node (nextSibling, nodeIs, nodeParent, prevSibling)
import Partial.Unsafe (unsafePartial)
import Test.Unit (TestSuite, test)
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

    Assert.equal 0 =<< liftEffect do
      missing <- docFind "missing/text()" doc
      pure $ length missing
