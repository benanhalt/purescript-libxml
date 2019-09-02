module Test.NodeAttribute (nodeAttributeTest) where

import Libxml.Types
import Prelude

import Data.Array (length, zipWith)
import Data.Maybe (fromJust, isNothing)
import Data.Traversable (for_, sequence)
import Effect.Class (liftEffect)
import Libxml.Attribute (attrValue)
import Libxml.Document (defaultDocEncodingAndVersion, docCreateRoot, newDoc)
import Libxml.Element (elementAttr, elementAttrs, elementSetAttr)
import Libxml.Node (nextSibling, nodeIs, nodeParent, prevSibling)
import Partial.Unsafe (unsafePartial)
import Test.Unit (TestSuite, test)
import Test.Unit.Assert as Assert


nodeAttributeTest :: TestSuite
nodeAttributeTest = do
  test "basic" do
    elem <- liftEffect do
      doc <- newDoc defaultDocEncodingAndVersion
      elem <- docCreateRoot "name" "" doc
      elementSetAttr "to" "wongfoo" elem
      pure elem

    Assert.equal "wongfoo" =<< liftEffect do
      attr <- unsafePartial fromJust <$> elementAttr "to" elem
      attrValue attr

  test "null" do
    elem <- liftEffect do
      doc <- newDoc defaultDocEncodingAndVersion
      docCreateRoot "name" "" doc

    Assert.assert "attribute is Nothing" =<< liftEffect do
      maybeAttr <- elementAttr "to" elem
      pure $ isNothing maybeAttr

  test "change" do
    elem <- liftEffect do
      doc <- newDoc defaultDocEncodingAndVersion
      elem <- docCreateRoot "name" "" doc
      elementSetAttr "to" "wongfoo" elem
      pure elem

    Assert.equal "wongfoo" =<< liftEffect do
      attr <- unsafePartial fromJust <$> elementAttr "to" elem
      attrValue attr

    liftEffect $ elementSetAttr "to" "julie newmar" elem

    Assert.equal "julie newmar" =<< liftEffect do
      attr <- unsafePartial fromJust <$> elementAttr "to" elem
      attrValue attr

  test "attrs" do
    elem <- liftEffect do
      doc <- newDoc defaultDocEncodingAndVersion
      docCreateRoot "root" "" doc

    Assert.equal 0 =<< (liftEffect $ length <$> elementAttrs elem)

    liftEffect do
      elementSetAttr "foo" "bar" elem
      elementSetAttr "bar" "baz" elem
      elementSetAttr "baz" "foo" elem

    attrs <- liftEffect $ sequence
             [ unsafePartial fromJust <$> elementAttr "foo" elem
             , unsafePartial fromJust <$> elementAttr "bar" elem
             , unsafePartial fromJust <$> elementAttr "baz" elem
             ]

    attrs' <- liftEffect $ elementAttrs elem
    checks <- liftEffect $ sequence $ zipWith nodeIs attrs attrs'
    for_ checks \check ->
      Assert.assert "the attrs should be the same node" check

  test "siblings" do
    elem <- liftEffect do
      doc <- newDoc defaultDocEncodingAndVersion
      elem <- docCreateRoot "root" "" doc
      elementSetAttr "foo" "bar" elem
      elementSetAttr "bar" "baz" elem
      elementSetAttr "baz" "foo" elem
      pure elem

    Assert.assert "baz should follow bar" =<< liftEffect do
      bar <- unsafePartial fromJust <$> elementAttr "bar" elem
      baz <- unsafePartial fromJust <$> elementAttr "baz" elem
      baz' <- unsafePartial fromJust <$> nextSibling bar
      nodeIs baz baz'

    Assert.assert "bar should follow foo" =<< liftEffect do
      bar <- unsafePartial fromJust <$> elementAttr "bar" elem
      foo <- unsafePartial fromJust <$> elementAttr "foo" elem
      foo' <- unsafePartial fromJust <$> prevSibling bar
      nodeIs foo foo'

    Assert.assert "foo should have no prev sibling" =<< liftEffect do
      foo <- unsafePartial fromJust <$> elementAttr "foo" elem
      prev <- prevSibling foo
      pure $ isNothing prev

    Assert.assert "baz should have no next sibling" =<< liftEffect do
      baz <- unsafePartial fromJust <$> elementAttr "baz" elem
      next <- nextSibling baz
      pure $ isNothing next

  test "getters" do
    elem <- liftEffect do
      doc <- newDoc defaultDocEncodingAndVersion
      elem <- docCreateRoot "root" "" doc
      elementSetAttr "foo" "bar" elem
      pure elem

    Assert.assert "parent of attr should be node" =<< liftEffect do
      attr <- unsafePartial fromJust <$> elementAttr "foo" elem
      parent <- unsafePartial fromJust <$> nodeParent attr
      nodeIs elem parent
