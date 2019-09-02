module Test.Element (elementTest) where

import Libxml.Document
import Libxml.Element
import Libxml.Node
import Libxml.Types
import Prelude

import Data.Array (index, length)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Effect.Class (liftEffect)
import Test.Unit (TestSuite, test)
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

    children <- liftEffect $ docFind "/name1/child" doc
    Assert.equal 1 $ length children

    liftEffect $ nodeRemove child

    children' <- liftEffect $ docFind "/name1/child" doc
    Assert.equal 0 $ length children'


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
    Assert.equal 1 =<< (liftEffect $ length <$> docFind "/name1/child" doc)

    liftEffect $ nodeRemove child
    name2 <- liftEffect $ elementAddNode "name2" "" elem
    liftEffect $ elementAddChild child name2
    Assert.equal 0 =<< (liftEffect $ length <$> docFind "/name1/child" doc)
    Assert.equal 1 =<< (liftEffect $ length <$> docFind "/name1/name2/child" doc)

  test "add child" do
    doc <- liftEffect do
      doc <- newDoc defaultDocEncodingAndVersion
      elem <- docCreateRoot "name1" "" doc
      newChild <- newElement doc "new-child" ""
      elementAddChild newChild elem
      pure doc
    Assert.equal 1 =<< (liftEffect $ length <$> docFind "/name1/new-child" doc)

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
