module Test.Searching (searchingTest) where

import Libxml.Types
import Prelude

import Data.Array (length, zipWith)
import Data.Either (fromRight)
import Data.Maybe (Maybe(..), fromJust)
import Data.Traversable (sequence)
import Effect.Class (liftEffect)
import Libxml (parseXmlString)
import Libxml.Attribute (attrNode, attrValue)
import Libxml.Document (defaultDocEncodingAndVersion, docCreateRoot, docFind, docGetAttr, docGetElement, docToString, newDoc)
import Libxml.Element (elementAddNode, elementAttr, elementGetAttr, elementGetElement, elementSetAttr)
import Libxml.Node (nodeIs)
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

    nodes <- liftEffect $ unsafePartial do
      (Just (NodeSet nodes)) <- docFind "missing/text()" doc
      pure $ nodes

    Assert.equal 0 $ length nodes


  test "get attribute" $ unsafePartial do
    doc <- liftEffect $ newDoc defaultDocEncodingAndVersion
    root <- liftEffect $ docCreateRoot "root" "" doc
    child <- liftEffect $ elementAddNode "child" "" root
    liftEffect $ elementSetAttr "attr" "val" child
    (Just attr) <- liftEffect $ elementAttr "attr" child

    Assert.assert "xpath gets attribute" =<< liftEffect do
      (Just attr') <- docGetAttr "//@attr" doc
      nodeIs attr attr'

    Assert.assert "nested" =<< liftEffect do
      (Just child') <- docGetElement "child" doc
      (Just attr') <- elementGetAttr "@attr" child'
      nodeIs attr attr'

    Assert.equal "val" =<< liftEffect do
      (Just child') <- docGetElement "child" doc
      (Just attr') <- elementGetAttr "@attr" child'
      attrValue attr'


    --check again after re-parsign the doc
    doc' <- liftEffect $ fromRight <$> (parseXmlString =<< docToString doc)

    Assert.equal "val" =<< liftEffect do
      (Just attr') <- docGetAttr "//@attr" doc'
      attrValue attr'

    Assert.equal "val" =<< liftEffect do
      (Just child') <- docGetElement "child" doc'
      (Just attr') <- elementGetAttr "@attr" child'
      attrValue attr'

    Assert.assert "attr should belong to child node" =<< liftEffect do
      (Just child') <- docGetElement "child" doc'
      (Just attr') <- docGetAttr "//@attr" doc'
      node <- attrNode attr'
      nodeIs node child'

  test "get non nodeset" do
    doc <- liftEffect $ newDoc defaultDocEncodingAndVersion
    root <- liftEffect $ docCreateRoot "root" "" doc

    Assert.equal true =<< (liftEffect <<< unsafePartial) do
      (Just (BoolResult r)) <- docFind "true()" doc
      pure r


    Assert.equal false =<< (liftEffect <<< unsafePartial) do
      (Just (BoolResult r)) <- docFind "false()" doc
      pure r

    Assert.equal "Hello, world!" =<< (liftEffect <<< unsafePartial) do
      (Just (StringResult r)) <- docFind "\"Hello, world!\"" doc
      pure r

    Assert.equal 1.23 =<< (liftEffect <<< unsafePartial) do
      (Just (NumberResult r)) <- docFind "1.23" doc
      pure r

  test "find" do
    doc <- liftEffect $ newDoc defaultDocEncodingAndVersion
    root <- liftEffect $ docCreateRoot "root" "" doc
    children <- liftEffect do
      child1 <- elementAddNode "child" "" root
      child2 <- elementAddNode "child" "" root
      pure [child1, child2]

    results <- unsafePartial $ liftEffect do
      (Just (NodeSet results)) <- docFind "child" doc
      pure results

    Assert.equal 2 $ length children
    Assert.equal 2 $ length results

    Assert.equal [true, true] =<< liftEffect do
      sequence $ zipWith nodeIs children results
