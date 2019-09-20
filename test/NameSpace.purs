module Test.NameSpace (nameSpaceTest) where

import Libxml.Types
import Prelude

import Data.Array (elemIndex, head, length, zipWith)
import Data.Either (Either(..), fromRight)
import Data.Maybe (Maybe(..), fromJust, isJust, isNothing)
import Data.Traversable (for_, sequence)
import Effect.Class (liftEffect)
import Libxml (parseXmlString)
import Libxml.Document (defaultDocEncodingAndVersion, docChildNodes, docCreateRoot, docFind, docFindWithNS, docGetRoot, docSetRoot, docToString, newDoc)
import Libxml.Element (elementAddChild, elementAddNextSibling, elementAddNode, elementAddPrevSibling, elementAllNS, elementClearNS, elementDefineNS, elementDefineNSwithPrefix, elementFind, elementNS, elementSetNShref, elementSetNShrefWithPrefix, newElement)
import Libxml.NameSpace (href, prefix)
import Libxml.Node (asText, nodeIs, nodeParent, nodeRemove, nodeToString, nodeType)
import Libxml.Text (newText, textAddNextSibling, textAddPrevSibling, textGetText, textSetText)
import Partial.Unsafe (unsafePartial)
import Test.Unit (TestSuite, failure, suite, test)
import Test.Unit.Assert as Assert


nameSpaceTest :: TestSuite
nameSpaceTest = do
  test "create" do
    doc <- liftEffect $ newDoc defaultDocEncodingAndVersion
    elem <- liftEffect $ docCreateRoot "name1" "" doc
    ns <- liftEffect $ elementDefineNS "http://my-namespace.com" elem
    Assert.assert "element namespace should be nothing" =<< liftEffect do
      ns' <- elementNS elem
      pure $ isNothing ns'

    Assert.assert "prefix is nothing" $ isNothing (prefix ns)
    Assert.equal "http://my-namespace.com" $ href ns

  test "assign namespace to a node" $ unsafePartial do
    doc <- liftEffect $ newDoc defaultDocEncodingAndVersion
    elem <- liftEffect $ docCreateRoot "name1" "" doc
    liftEffect $ elementSetNShref "http://my-namespace.com" elem
    (Just ns) <- liftEffect $ elementNS elem
    Assert.equal Nothing $ prefix ns
    Assert.equal "http://my-namespace.com" $ href ns

  test "with prefix" $ unsafePartial do
    doc <- liftEffect $ newDoc defaultDocEncodingAndVersion
    elem <- liftEffect $ docCreateRoot "name1" "" doc
    ns <- liftEffect $ elementDefineNSwithPrefix "pref" "http://my-namespace.com" elem
    Assert.assert "elment namespace should be nothing " =<< liftEffect do
      ns' <- elementNS elem
      pure $ isNothing ns'
    Assert.equal (Just "pref") $ prefix ns
    Assert.equal "http://my-namespace.com" $ href ns

    liftEffect $ elementSetNShrefWithPrefix "pref" "http://my-namespace.com" elem
    (Just ns') <- liftEffect $ elementNS elem
    Assert.equal (Just "pref") $ prefix ns'
    Assert.equal "http://my-namespace.com" $ href ns'

  suite "from parsing" do
    test "from parsing" $ unsafePartial do
      (Right doc) <- liftEffect $ parseXmlString """<?xml version="1.0" encoding="UTF-8"?><name1 xmlns="http://my-namespace.com"/>"""
      (Just elem) <- liftEffect $ docGetRoot doc
      (Just ns) <- liftEffect $ elementNS elem
      Assert.equal Nothing $ prefix ns
      Assert.equal "http://my-namespace.com" $ href ns

    test "no prefix from parsing" $ unsafePartial do
      (Right doc) <- liftEffect $ parseXmlString """<?xml version="1.0" encoding="UTF-8"?><name1 xmlns:pref="http://my-namespace.com"/>"""
      (Just elem) <- liftEffect $ docGetRoot doc
      ns <- liftEffect $ elementNS elem
      Assert.assert "ns should be Nothing" $ isNothing ns

    test "with prefix" $ unsafePartial do
      (Right doc) <- liftEffect $ parseXmlString """<?xml version="1.0" encoding="UTF-8"?><pref:name1 xmlns:pref="http://my-namespace.com"/>"""
      (Just elem) <- liftEffect $ docGetRoot doc
      (Just ns) <- liftEffect $ elementNS elem
      Assert.equal (Just "pref") $ prefix ns
      Assert.equal "http://my-namespace.com" $ href ns

    test "remove" do
      doc <- liftEffect $ newDoc defaultDocEncodingAndVersion
      elem <- liftEffect $ docCreateRoot "name1" "" doc
      liftEffect $ elementSetNShref "http://my-namespace.com" elem
      Assert.assert "namespace should exist" =<< liftEffect do
        ns <- elementNS elem
        pure $ isJust ns

      liftEffect $ elementClearNS elem
      Assert.assert "namespace should not exist" =<< liftEffect do
        ns <- elementNS elem
        pure $ isNothing ns

    test "all" $ unsafePartial do
      doc <- liftEffect $ newDoc defaultDocEncodingAndVersion
      elem <- liftEffect $ docCreateRoot "root" "" doc

      liftEffect $ elementSetNShrefWithPrefix "com" "http://example.com" elem
      (Just ns1) <- liftEffect $ elementNS elem

      liftEffect $ elementSetNShrefWithPrefix "net" "http://example.net" elem
      (Just ns2) <- liftEffect $ elementNS elem

      liftEffect $ elementSetNShref "http://example.org" elem
      (Just ns3) <- liftEffect $ elementNS elem

      nss <- liftEffect $ elementAllNS elem
      Assert.equal 3 (length nss)
      void $ sequence $ zipWith (
        \ns ns' -> do
          Assert.equal (href ns) (href ns')
          Assert.equal (prefix ns) (prefix ns')
        ) [ns1, ns2, ns3] nss

    test "empty" do
      doc <- liftEffect $ newDoc defaultDocEncodingAndVersion
      elem <- liftEffect $ docCreateRoot "root" "" doc

      Assert.equal 0 =<< (liftEffect $ length <$> elementAllNS elem)

    test "nested" do
      doc <- liftEffect $ newDoc defaultDocEncodingAndVersion
      root <- liftEffect $ docCreateRoot "root" "" doc

      liftEffect $ elementSetNShrefWithPrefix "com" "http://example.com" root
      Assert.equal 1 =<< (liftEffect $ length <$> elementAllNS root)

      child <- liftEffect $ elementAddNode "child" "" root
      liftEffect $ elementSetNShrefWithPrefix "net" "http://example.net" child
      Assert.equal 2 =<< (liftEffect $ length <$> elementAllNS child)

      liftEffect $ elementSetNShref "http://example.org" root
      Assert.equal 3 =<< (liftEffect $ length <$> elementAllNS child)

    test "xmlns" $ unsafePartial do
      (Right doc) <- liftEffect $ parseXmlString
                     """<html xmlns="http://www.w3.org/1999/xhtml"><head></head><body><div>BACON</div><div>ROCKS</div><p>WUT?</p></body></html>"""
      (Just (NodeSet divs)) <- liftEffect $ docFindWithNS "//xmlns:div" "http://www.w3.org/1999/xhtml" doc
      Assert.equal 2 $ length divs
