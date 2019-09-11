module Test.Text (textTest) where

import Libxml.Types
import Prelude

import Data.Array (head, length)
import Data.Either (fromRight)
import Data.Maybe (Maybe(..), fromJust, isNothing)
import Data.Traversable (sequence)
import Effect.Class (liftEffect)
import Libxml (parseXmlString)
import Libxml.Document (defaultDocEncodingAndVersion, docChildNodes, docSetRoot, docToString, newDoc)
import Libxml.Element (elementAddChild, elementAddNextSibling, elementAddNode, elementAddPrevSibling, newElement)
import Libxml.Node (asText, nodeIs, nodeParent, nodeRemove, nodeToString, nodeType)
import Libxml.Text (newText, textAddNextSibling, textAddPrevSibling, textGetText, textSetText)
import Partial.Unsafe (unsafePartial)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert


textTest :: TestSuite
textTest = do
  test "new comment" do
    doc <- liftEffect $ newDoc defaultDocEncodingAndVersion
    elem <- liftEffect $ newText "node content" doc
    liftEffect $ docSetRoot elem doc
    Assert.equal "node content" =<< (liftEffect $ textGetText elem)

  test "setters" do
    doc <- liftEffect $ newDoc defaultDocEncodingAndVersion
    elem <- liftEffect $ newText "node content" doc

    Assert.equal "node content" =<< (liftEffect $ textGetText elem)
    liftEffect $ textSetText "content && more content <>" elem
    Assert.equal "content &amp;&amp; more content &lt;&gt;" =<<
      (liftEffect $ textGetText elem)

  test "getters" do
    doc <- liftEffect $ newDoc defaultDocEncodingAndVersion
    elem <- liftEffect $ newText "node content" doc
    Assert.equal Text $ nodeType elem

  test "remove" do
    doc <- liftEffect $ newDoc defaultDocEncodingAndVersion
    elem <- liftEffect $ newText "node content" doc
    liftEffect $ docSetRoot elem doc

    Assert.equal "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\nnode content\n" =<<
      (liftEffect $ docToString doc)

    liftEffect $ nodeRemove elem

    Assert.equal "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" =<<
      (liftEffect $ docToString doc)

  test "to string" do
    doc <- liftEffect $ newDoc defaultDocEncodingAndVersion
    elem <- liftEffect $ newText "node content" doc
    liftEffect $ docSetRoot elem doc

    Assert.equal "node content" =<< (liftEffect $ nodeToString elem)

  test "add child" do
    doc <- liftEffect $ newDoc defaultDocEncodingAndVersion
    newTextNode <- liftEffect $ newText "my text" doc
    newElement <- liftEffect $ newElement doc "div" ""

    liftEffect $ elementAddChild newTextNode newElement
    liftEffect $ docSetRoot newElement doc

    Assert.equal "<div>my text</div>" =<<
      (liftEffect $ nodeToString newElement)

  test "add siblings" do
    parentNode <- liftEffect do
      doc <-newDoc defaultDocEncodingAndVersion
      parentNode <- newElement doc "div" ""
      child <- elementAddNode "child" "i'm a child" parentNode
      prevTextNode <- newText "before text" doc
      nextTextNode <- newText "after text" doc

      elementAddPrevSibling prevTextNode child
      elementAddNextSibling nextTextNode child

      pure parentNode

    Assert.equal "<div>before text<child>i'm a child</child>after text</div>" =<<
      (liftEffect $ nodeToString parentNode)

  test "add prev sibling merge text" $ unsafePartial do
    doc <- liftEffect $ fromRight <$> parseXmlString "<foo>bar<baz/></foo>"
    (Just bar) <- liftEffect do
      nodes <- docChildNodes doc
      pure $ asText =<< head nodes

    qux <- liftEffect $ newText "qux" doc
    liftEffect $ textAddPrevSibling qux bar

    children <- liftEffect $ docChildNodes doc
    Assert.equal 2 $ length children

    Assert.equal (Just "quxbar") =<< (liftEffect $
      sequence $ textGetText <$> (asText =<< head children))

    Assert.assertFalse "added text should merged into existing node" =<< liftEffect do
      nodeIs qux $ fromJust $ head children

    Assert.assert "qux parent should be Nothing" =<< liftEffect do
      parent <- nodeParent qux
      pure $ isNothing parent

    Assert.equal "qux" =<< liftEffect (textGetText qux)


  test "add next sibling merge text" $ unsafePartial do
    doc <- liftEffect $ fromRight <$> parseXmlString "<foo>bar<baz/></foo>"
    (Just bar) <- liftEffect do
      nodes <- docChildNodes doc
      pure $ asText =<< head nodes

    qux <- liftEffect $ newText "qux" doc
    liftEffect $ textAddNextSibling qux bar

    children <- liftEffect $ docChildNodes doc
    Assert.equal 2 $ length children

    Assert.equal (Just "barqux") =<< (liftEffect $
      sequence $ textGetText <$> (asText =<< head children))

    Assert.assertFalse "added text should merged into existing node" =<< liftEffect do
      nodeIs qux $ fromJust $ head children

    Assert.assert "qux parent should be Nothing" =<< liftEffect do
      parent <- nodeParent qux
      pure $ isNothing parent

    Assert.equal "qux" =<< liftEffect (textGetText qux)

  suite "text node" do
    test "text" $ unsafePartial do
      doc <- liftEffect $ fromRight <$> parseXmlString "<?xml version=\"1.0\"?><root>child</root>"
      Assert.equal Text =<< liftEffect do
        (Just child0) <- head <$> docChildNodes doc
        pure $ nodeType child0

    test "comment" $ unsafePartial do
      doc <- liftEffect $ fromRight <$> parseXmlString "<?xml version=\"1.0\"?><root><!-- comment --></root>"
      Assert.equal Comment =<< liftEffect do
        (Just child0) <- head <$> docChildNodes doc
        pure $ nodeType child0

    test "cdata" $ unsafePartial do
      doc <- liftEffect $ fromRight <$> parseXmlString "<?xml version=\"1.0\"?><root><![CDATA[cdata text]]></root>"
      Assert.equal CData =<< liftEffect do
        (Just child0) <- head <$> docChildNodes doc
        pure $ nodeType child0
