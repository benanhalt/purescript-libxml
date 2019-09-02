module Test.Document (documentTest) where

import Libxml.Document
import Libxml.Element
import Prelude

import Data.Array (head, length)
import Data.Either (Either(..), fromRight)
import Data.Maybe (Maybe(..), fromJust, fromMaybe, isNothing)
import Data.Traversable (sequence, traverse)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Libxml (parseXmlString)
import Libxml.DTD (dtdExternalId, dtdName, dtdSystemId)
import Libxml.Node (asElement, nodeParent)
import Partial.Unsafe (unsafePartial)
import Test.Unit (TestSuite, failure, suite, suiteSkip, test)
import Test.Unit.Assert as Assert


documentTest :: TestSuite
documentTest = do
  getDtd
  suiteSkip "skip" $ suite "setDtd" do
    test "write tests" do failure "tests not written"
  newDocument

getDtd :: TestSuite
getDtd =
  suite "getDtd" do
    test "getDtd with no dtd" do
      doc <- liftEffect $ parseXmlString """<?xml version="1.0" encoding="UTF-8"?>
<root></root>
"""
      case doc of
        (Left err) -> failure $ show err
        (Right doc') -> do
          dtd <- liftEffect $ docGetDtd doc'
          Assert.assert "dtd should be Nothing" $ isNothing dtd

    test "getDtd html" do
        doc <- liftEffect $ parseXmlString """<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE html>
<root></root>
"""
        case doc of
         (Left err) -> failure $ show err
         (Right doc') -> do
           dtd' <- liftEffect $ docGetDtd doc'
           case dtd' of
             Nothing -> failure "dtd was Nothing"
             (Just dtd) -> do
               Assert.equal "html" (dtdName dtd)
               Assert.assert "dtd external id should be Nothing" $ isNothing $ dtdExternalId dtd
               Assert.assert "dtd system id should be Nothing" $ isNothing $ dtdSystemId dtd

    test "getDtd html with systemId" do
        doc <- liftEffect $ parseXmlString """<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE html SYSTEM "http://www.w3.org/TR/html4/strict.dtd">
<root></root>
"""
        case doc of
         (Left err) -> failure $ show err
         (Right doc') -> do
           dtd' <- liftEffect $ docGetDtd doc'
           case dtd' of
             Nothing -> failure "dtd was Nothing"
             (Just dtd) -> do
               Assert.equal "html" (dtdName dtd)
               Assert.assert "dtd external id should be Nothing" $ isNothing $ dtdExternalId dtd
               Assert.equal (Just "http://www.w3.org/TR/html4/strict.dtd") (dtdSystemId dtd)

    test "getDtd html with systemId and externalId" do
        doc <- liftEffect $ parseXmlString """<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01//EN" "http://www.w3.org/TR/html4/strict.dtd">
<root></root>
"""
        case doc of
         (Left err) -> failure $ show err
         (Right doc') -> do
           dtd' <- liftEffect $ docGetDtd doc'
           case dtd' of
             Nothing -> failure "dtd was Nothing"
             (Just dtd) -> do
               Assert.equal "html" (dtdName dtd)
               Assert.equal (Just "-//W3C//DTD HTML 4.01//EN") (dtdExternalId dtd)
               Assert.equal (Just "http://www.w3.org/TR/html4/strict.dtd") (dtdSystemId dtd)


newDocument :: TestSuite
newDocument = suite "new document" do
  test "defaults" do
    doc <- liftEffect $ newDoc defaultDocEncodingAndVersion
    version <- liftEffect $ docVersion doc
    Assert.equal "1.0" version
    encoding <- liftEffect $ docEncoding doc
    Assert.equal "utf8" encoding

  test "with set version and encoding" do
    doc <- liftEffect $ newDoc defaultDocEncodingAndVersion {version = "2.0", encoding = "UTF-8"}
    version <- liftEffect $ docVersion doc
    Assert.equal "2.0" version
    encoding <- liftEffect $ docEncoding doc
    Assert.equal "UTF-8" encoding

  test "null root" do
    doc <- liftEffect $ newDoc defaultDocEncodingAndVersion
    root <- liftEffect $ docGetRoot doc
    Assert.assert "root should be Nothing" $ isNothing root

  test "new root" do
    doc <- liftEffect $ newDoc defaultDocEncodingAndVersion
    root <- liftEffect $ docCreateRoot "root" "" doc
    name <- liftEffect $ elementName root
    Assert.equal "root" name
    root' <- liftEffect $ docGetRoot doc
    case root' of
      Nothing -> failure "root node missing"
      Just root'' -> do
        name' <- liftEffect $ elementName root''
        Assert.equal "root" name'

  test "one child" do
    doc <- liftEffect $ newDoc defaultDocEncodingAndVersion
    root <- liftEffect $ docCreateRoot "root" "" doc
    child <- liftEffect $ elementAddNode "child" "" root
    parent' <- liftEffect $ nodeParent child
    case parent' of
      Nothing -> failure "child should have parent"
      Just parent -> case asElement parent of
        Nothing -> failure "parent should be element"
        Just parentElement -> do
          void $ liftEffect $ elementAddNode "child" "" parentElement
    rootName <- liftEffect $ traverse elementName =<< docGetRoot doc
    Assert.equal (Just "root") rootName
    rootName' <- liftEffect ((traverse elementName <<< head) =<< docFind "/root" doc)
    Assert.equal (Just "root") rootName'

  test "root children" do
    names <- liftEffect do
      doc <- newDoc defaultDocEncodingAndVersion
      root <- docCreateRoot "root" "" doc
      child1 <- elementAddNode "child-one" "" root
      root' <- unsafePartial $ (fromJust <<< (=<<) asElement) <$> nodeParent child1
      _ <- elementAddNode "child-two" "" root'
      children <- docChildNodes doc
      sequence $ (sequence <<< map elementName <<< asElement) <$> children

    Assert.equal (map Just ["child-one", "child-two"]) names

  test "xpath" do
    doc <- liftEffect do
      doc <- newDoc defaultDocEncodingAndVersion
      root <- docCreateRoot "root" "" doc
      child1 <- elementAddNode "child" "" root
      child2 <- elementAddNode "child" "" root
      pure doc
    children <- liftEffect $ docFind "child" doc
    Assert.equal 2 $ length children

  test "xpath child" do
    doc <- liftEffect do
      doc <- newDoc defaultDocEncodingAndVersion
      root <- docCreateRoot "root" "" doc
      _ <- elementAddNode "child-one" "" root
      _ <- elementAddNode "child-two" "" root
      pure doc
    child1Name <- liftEffect $ docFind "child-one" doc >>= map elementName >>> sequence <#> head
    Assert.equal (Just "child-one") child1Name
    child2Name <- liftEffect $ docFind "child-two" doc >>= map elementName >>> sequence <#> head
    Assert.equal (Just "child-two") child2Name

  test "toString" do
    doc <- liftEffect do
      doc <- newDoc defaultDocEncodingAndVersion
      root <- docCreateRoot "root" "" doc
      child <- elementAddNode "child" "" root
      elementSetAttr "to" "wongfoo" child
      grandchild <- elementAddNode "grandchild" "with love" child
      elementSetAttr "from" "julie numar" grandchild
      sibling <- elementAddNode "sibling" "with content!" root
      pure doc
    (liftEffect $ docToString doc) >>= Assert.equal """<?xml version="1.0" encoding="UTF-8"?>
<root>
  <child to="wongfoo">
    <grandchild from="julie numar">with love</grandchild>
  </child>
  <sibling>with content!</sibling>
</root>
"""

  test "add child nodes" do
    doc1 <- liftEffect $ unsafePartial $ fromRight <$>
            parseXmlString """<?xml version="1.0" encoding="UTF-8"?>
<root><child to="wongfoo"><grandchild from="julie numar">with love</grandchild></child><sibling>with content!</sibling></root>
"""
    doc2 <- liftEffect $ unsafePartial $ fromRight <$>
            parseXmlString """<?xml version="1.0" encoding="UTF-8"?>
<root><child to="wongfoo"></child><sibling>with content!</sibling></root>
"""
    liftEffect do
      doc2Child0 <- unsafePartial $ (fromJust <<< (=<<) asElement <<< head) <$> docChildNodes doc2
      doc1Child0 <- unsafePartial $ (fromJust <<< (=<<) asElement <<< head) <$> docChildNodes doc1
      doc1Child00 <- unsafePartial $ (fromJust <<< (=<<) asElement <<< head) <$> elementChildNodes doc1Child0
      elementAddChild doc1Child00 doc2Child0
    doc1String <- liftEffect $ docToString doc1
    doc2String <- liftEffect $ docToString doc2
    Assert.equal doc1String doc2String


-- module.exports.add_cdata_nodes = function(assert) {
--     var gchild = '';
--     var doc1_string = [
--       '<?xml version="1.0" encoding="UTF-8"?>',
--       '<root><child to="wongfoo"/></root>',
--     ].join("\n");

--     var expected_string = [
--       '<?xml version="1.0" encoding="UTF-8"?>',
--       '<root>',
--       '  <child to="wongfoo"><![CDATA[<p>Bacon</p>]]></child>',
--       '</root>',
--       '' /* Why?!? */
--     ].join("\n");

--     var doc1 = libxml.parseXml(doc1_string);
--     doc1.child(0).cdata('<p>Bacon</p>');
--     assert.equal(doc1.toString(), expected_string);
--     assert.done();
-- };

