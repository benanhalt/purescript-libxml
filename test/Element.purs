module Test.Element (elementTest) where

import Libxml.Document
import Libxml.Element
import Libxml.Node
import Libxml.Types
import Prelude

import Data.Array (head, index, length)
import Data.Either (fromRight)
import Data.Maybe (Maybe(..), fromJust, isJust, isNothing)
import Data.Traversable (sequence, traverse)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Libxml (parseXmlString)
import Libxml.Text (newText, textGetText)
import Partial.Unsafe (unsafePartial)
import Test.Unit (TestSuite, failure, suite, test, testSkip)
import Test.Unit.Assert as Assert

elementTest :: TestSuite
elementTest = do
  test "new element" do
    doc <- liftEffect $ newDoc defaultDocEncodingAndVersion
    elem <- liftEffect $ newElement doc "name1" ""
    liftEffect $ docSetRoot elem doc
    elemName <- liftEffect $ elementName elem
    Assert.equal "name1" elemName
    root <- liftEffect $ docGetRoot doc
    rootName <- liftEffect $ sequence $ elementName <$> (asElement =<< root)
    Assert.equal (Just "name1") rootName

  test "new element with content" do
    doc <- liftEffect $ newDoc defaultDocEncodingAndVersion
    elem <- liftEffect $ newElement doc "name1" "content && more content <>"
    liftEffect $ docSetRoot elem doc
    elemName <- liftEffect $ elementName elem
    Assert.equal "name1" elemName
    root <- liftEffect $ docGetRoot doc
    rootName <- liftEffect $ sequence $ elementName <$> (asElement =<< root)
    Assert.equal (Just "name1") rootName
    elemText <- liftEffect $ elementText elem
    Assert.equal "content && more content <>" elemText

  test "setters" do
    doc <- liftEffect $ newDoc defaultDocEncodingAndVersion
    elem <- liftEffect $  newElement doc "name1" ""

    -- change content
    elemText <- liftEffect $ elementText elem
    Assert.equal "" elemText
    liftEffect $ elementSetText "content && more content <>" elem
    elemText' <- liftEffect $ elementText elem
    Assert.equal "content && more content <>" elemText'

    -- change name
    elemName <- liftEffect $ elementName elem
    Assert.equal "name1" elemName
    liftEffect $ elementSetName "newname" elem
    elemName' <- liftEffect $ elementName elem
    Assert.equal "newname" elemName'

  test "getters" do
    doc <- liftEffect $ newDoc defaultDocEncodingAndVersion
    elem <- liftEffect $  newElement doc "name1" ""
    Assert.equal Element $ nodeType elem

  test "remove" do
    doc <- liftEffect $ newDoc defaultDocEncodingAndVersion
    elem <- liftEffect $ docCreateRoot "name1" "" doc
    child <- liftEffect $ elementAddNode "child" "" elem

    Assert.assert "element should be found" =<< liftEffect do
      el <- docGetElement "/name1/child" doc
      pure $ isJust el

    liftEffect $ nodeRemove child

    Assert.assert "element should not be found" =<< liftEffect do
      el <- docGetElement "/name1/child" doc
      pure $ isNothing el

  test "toString" do
    doc <- liftEffect $ newDoc defaultDocEncodingAndVersion
    elem <- liftEffect $ docCreateRoot "name1" "" doc
    Assert.equal "<name1/>" =<< (liftEffect $ nodeToString elem)

    _ <- liftEffect $ elementAddNode "child" "" elem
    Assert.equal "<name1><child/></name1>" =<< (liftEffect $ nodeToString elem)

  test "path" do
    doc <- liftEffect $ newDoc defaultDocEncodingAndVersion
    root <- liftEffect $ docCreateRoot "root" "" doc
    child <- liftEffect $ elementAddNode "child" "" root
    gchild <- liftEffect $ elementAddNode "grandchild" "" child
    sibling <- liftEffect $ elementAddNode "child" "" root

    Assert.equal "/root/child[1]/grandchild" =<< (liftEffect $ elementPath gchild)
    Assert.equal "/root/child[2]" =<< (liftEffect $ elementPath sibling)

  test "move" do
    doc <- liftEffect $ newDoc defaultDocEncodingAndVersion
    elem <- liftEffect $ docCreateRoot "name1" "" doc
    child <- liftEffect $ elementAddNode "child" "" elem
    Assert.assert "element 'child' should be found under 'name1'" =<< liftEffect do
      el <- docGetElement "/name1/child" doc
      pure $ isJust el

    liftEffect $ nodeRemove child
    name2 <- liftEffect $ elementAddNode "name2" "" elem
    liftEffect $ elementAddChild child name2
    Assert.assert "element 'child' should not be found under 'name1'" =<< liftEffect do
      el <- docGetElement "/name1/child" doc
      pure $ isNothing el
    Assert.assert "element 'child' should be found under 'name2'" =<< liftEffect do
      el <- docGetElement "/name1/name2/child" doc
      pure $ isJust el

  test "add child" do
    doc <- liftEffect do
      doc <- newDoc defaultDocEncodingAndVersion
      elem <- docCreateRoot "name1" "" doc
      newChild <- newElement doc "new-child" ""
      elementAddChild newChild elem
      pure doc
    Assert.assert "new element should be found" =<< liftEffect do
      el <- docGetElement "/name1/new-child" doc
      pure $ isJust el

  test "add prev sibling" do
    doc <- liftEffect $ newDoc defaultDocEncodingAndVersion
    elem <- liftEffect $ docCreateRoot "name1" "" doc
    child1 <- liftEffect $ elementAddNode "child1" "" elem
    child2 <- liftEffect $ elementAddNode "child2" "" elem
    Assert.equal 2 =<< (liftEffect $ length <$> elementChildNodes elem)

    prevSibling <- liftEffect $ newElement doc "prev-sibling" ""
    liftEffect $ elementAddPrevSibling prevSibling child2
    children <- liftEffect $ elementChildNodes elem
    Assert.equal 3 $ length children
    let node = asElement =<< index children 1
    Assert.equal (Just "prev-sibling") =<< (liftEffect $ sequence $ elementName <$> node)

  test "add next sibling" do
    doc <- liftEffect $ newDoc defaultDocEncodingAndVersion
    elem <- liftEffect $ docCreateRoot "name1" "" doc
    child1 <- liftEffect $ elementAddNode "child1" "" elem
    child2 <- liftEffect $ elementAddNode "child2" "" elem
    Assert.equal 2 =<< (liftEffect $ length <$> elementChildNodes elem)

    nextSibling <- liftEffect $ newElement doc "next-sibling" ""
    addedSibling <- liftEffect $ elementAddNextSibling nextSibling child1
    children <- liftEffect $ elementChildNodes elem
    Assert.equal 3 $ length children
    let node = asElement =<< index children 1
    Assert.equal (Just "next-sibling") =<< (liftEffect $ sequence $ elementName <$> node)

  test "import" do
    doc <- liftEffect $ newDoc defaultDocEncodingAndVersion {encoding = "latin1"}
    elem <- liftEffect $ docCreateRoot "name1" "" doc
    child1 <- liftEffect $ elementAddNode "child1" "" elem
    doc' <- liftEffect $ unsafePartial $ fromJust <$> nodeDoc child1
    newdoc <- liftEffect $ newDoc defaultDocEncodingAndVersion
    _ <- liftEffect $ docCreateRoot "newdoc" "" newdoc
    liftEffect $ elementAddChild child1 =<< unsafePartial fromJust <$> docGetRoot newdoc
    doc'Encoding <- liftEffect $ docEncoding doc'
    newdocEncoding <- liftEffect $ docEncoding newdoc
    Assert.assertFalse "the docs should be different" $ doc'Encoding == newdocEncoding
    child1' <- liftEffect $ unsafePartial $ fromJust <$> head <$>
               (elementChildNodes =<<  fromJust <$> docGetRoot newdoc)
    Assert.equal (Just "child1") =<<
      (liftEffect $ sequence $ elementName <$> asElement child1')
    child1parentName <- liftEffect do
      p <- ((=<<) asElement) <$> nodeParent child1
      traverse elementName p
    Assert.equal (Just "name1") child1parentName

  test "clone" do
    doc <- liftEffect $ newDoc defaultDocEncodingAndVersion
    elem <- liftEffect $ docCreateRoot "child" "content" doc
    elem2 <- liftEffect $ nodeClone elem

    namesEqual <- liftEffect do
      elemName <- elementName elem
      elem2Name <- elementName elem2
      pure $ elemName == elem2Name
    Assert.assert "names are equal" namesEqual

    textsEqual <- liftEffect do
      text <- elementText elem
      text2 <- elementText elem2
      pure $ text == text2
    Assert.assert "text content are equal" textsEqual

    stringsEqual <- liftEffect do
      string <- nodeToString elem
      string2 <- nodeToString elem2
      pure $ string == string2
    Assert.assert "toStrings are equal" stringsEqual

  testSkip "namespace" do
    failure "not implemented"

  suite "replace" do
    let xml = "<foo>some <bar/> evening</foo>"

    test "with text" do
      docText <- liftEffect do
        doc <- unsafePartial fromRight <$> parseXmlString xml
        bar <- unsafePartial fromJust <$> docGetElement "bar" doc
        elementReplaceWithText "enchanted" bar
        root <- unsafePartial fromJust <$> docGetRoot doc
        elementText root
      Assert.equal "some enchanted evening" docText

    test "escaped text" do
      asString <- liftEffect do
        doc <- unsafePartial fromRight <$> parseXmlString xml
        bar <- unsafePartial fromJust <$> docGetElement "bar" doc
        elementReplaceWithText "<>" bar
        root <- unsafePartial fromJust <$> docGetRoot doc
        nodeToString root
      Assert.equal "<foo>some &lt;&gt; evening</foo>" asString

    test "with other element" do
      root <- liftEffect do
        doc <- unsafePartial fromRight <$> parseXmlString xml
        bar <- unsafePartial fromJust <$> docGetElement "bar" doc
        enchant <- unsafePartial fromRight <$> parseXmlString "<enchanted/>"
        enchantedRoot <- unsafePartial fromJust <$> docGetRoot enchant
        elementReplaceWithElement enchantedRoot bar
        unsafePartial fromJust <$> docGetRoot doc

      Assert.equal "<foo>some <enchanted/> evening</foo>" =<<
        (liftEffect $ nodeToString root)

      Assert.equal 3 =<< (liftEffect $ length <$> elementChildNodes root)

      Assert.equal "enchanted" =<< liftEffect do
        children <- elementChildNodes root
        elementName $ unsafePartial fromJust $ asElement =<< index children 1

  test "add child merge text" do
    doc <- liftEffect $ unsafePartial fromRight <$> parseXmlString "<foo>bar</foo>"
    foo <- liftEffect $ unsafePartial fromJust <$> docGetRoot doc
    baz <- liftEffect $ newText "baz" doc
    liftEffect $ elementAddChild baz foo

    Assert.equal "barbaz" =<< (liftEffect $ elementText foo)
    Assert.equal 1 =<< (liftEffect $ length <$> elementChildNodes foo)

    Assert.assertFalse "baz should not be child of foo" =<< liftEffect do
      fooChild <- unsafePartial fromJust <$> head <$> elementChildNodes foo
      nodeIs fooChild baz

    -- Assert.assert "doc should be parent of baz" =<< liftEffect do
    --   bazParent <- unsafePartial fromJust <$> nodeParent baz
    --   nodeIs bazParent doc -- Document is not a type of node!

    Assert.equal "baz" =<< (liftEffect $ textGetText baz)

