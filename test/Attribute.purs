module Test.Attribute (attributeTest) where

import Libxml.Types
import Prelude

import Data.Either (fromRight)
import Data.Maybe (Maybe(..), fromJust, isNothing)
import Data.Traversable (sequence, traverse)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Libxml (parseXmlString)
import Libxml.Attribute (attrName, attrNextSibling, attrNode, attrPrevSibling, attrSetValue, attrValue)
import Libxml.Document (docGetElement)
import Libxml.Element (elementAttr, elementName, elementSetAttr)
import Libxml.Node (nodeRemove, nodeType)
import Partial.Unsafe (unsafePartial)
import Test.Unit (TestSuite, failure, test, testSkip)
import Test.Unit.Assert as Assert

body :: String
body = """<?xml version='1.0' encoding='UTF-8'?>
<root><node attr-one-key='attr-one-value' attr-two-key='attr-two-value' attr-three-key='attr-three-value' /></root>
"""

attributeTest :: TestSuite
attributeTest = do
  test "new attribute" do
    node <- liftEffect do
      doc <- unsafePartial fromRight <$> parseXmlString body
      node <- unsafePartial fromJust <$> docGetElement "node" doc
      elementSetAttr "new-attr-key" "new-attr-value" node
      pure node

    Assert.equal (Just "new-attr-value") =<< liftEffect do
      attr <- elementAttr "new-attr-key" node
      sequence $ attrValue <$> attr

  testSkip "create with namespace" do
    failure "not implemented"

  test "getters" do
    node <- liftEffect do
      doc <- unsafePartial fromRight <$> parseXmlString body
      unsafePartial fromJust <$> docGetElement "node" doc

    Assert.equal (Just "attr-one-key") =<< liftEffect do
      attr <- elementAttr "attr-one-key" node
      pure $ attrName <$> attr

    Assert.equal (Just "attr-one-value") =<< liftEffect do
      attr <- elementAttr "attr-one-key" node
      sequence $ attrValue <$> attr

    Assert.equal (Just "node") =<< liftEffect do
      attr <- elementAttr "attr-one-key" node
      node' <- sequence $ attrNode <$> attr
      sequence $ elementName <$> node'

    Assert.equal (Just Attribute) =<< liftEffect do
      attr <- elementAttr "attr-two-key" node
      pure $ nodeType <$> attr

    -- siblings
    Assert.equal "attr-one-key" =<< liftEffect do
      attr <- unsafePartial fromJust <$> elementAttr "attr-two-key" node
      prev <- unsafePartial fromJust <$> attrPrevSibling attr
      pure $ attrName prev

    Assert.equal "attr-three-key" =<< liftEffect do
      attr <- unsafePartial fromJust <$> elementAttr "attr-two-key" node
      next <- unsafePartial fromJust <$> attrNextSibling attr
      pure $ attrName next

  test "setters" do
    node <- liftEffect do
      doc <- unsafePartial fromRight <$> parseXmlString body
      unsafePartial fromJust <$> docGetElement "node" doc

    liftEffect do
      attr <- unsafePartial fromJust <$> elementAttr "attr-one-key" node
      attrSetValue "new-value" attr

    Assert.equal "new-value" =<< liftEffect do
      attr <- unsafePartial fromJust <$> elementAttr "attr-one-key" node
      attrValue attr

  test "remove" do
    node <- liftEffect do
      doc <- unsafePartial fromRight <$> parseXmlString body
      unsafePartial fromJust <$> docGetElement "node" doc

    attr <- liftEffect $ unsafePartial fromJust <$> elementAttr "attr-one-key" node

    Assert.assertFalse "attribute should exist" =<< liftEffect do
      maybeAttr <- elementAttr "attr-one-key" node
      pure $ isNothing maybeAttr

    liftEffect $ nodeRemove attr

    Assert.assert "attribute should not exist" =<< liftEffect do
      maybeAttr <- elementAttr "attr-one-key" node
      pure $ isNothing maybeAttr
