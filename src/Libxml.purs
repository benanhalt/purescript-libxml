module Libxml
       ( Document
       , Node
       , Element
       , Comment
       , Attribute
       , DTD
       , RawNode
       , RawElementNode
       , RawCommentNode
       , RawAttributeNode
       , RawDTDNode
       , NodeType
       , DocEncodingAndVersion
       , defaultDocEncodingAndVersion
       , libxmljs_version
       , libxml_version
       , libxml_parser_version
       , memoryUsage
       , nodeCount
       , newDoc
       , docGetRoot
       , docCreateRoot
       , docFind
       , docEncoding
       , docVersion
       , docToString
       , docSetEncoding
       , docChildNodes
       , docGetDtd
       , elementAddNode
       , elementSetAttr
       , elementAddChild
       , elementChildNodes
       , parseXmlString
       , parseHtmlString
       , parseHtmlFragment
       , nodeType
       , nodeParent
       , asElement
       , dtdName
       , dtdExternalId
       , dtdSystemId
       , elementName
       , elementText
       )
where

import Prelude

import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import Effect.Exception (Error, try)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, runEffectFn1, runEffectFn2, runEffectFn3)
import Partial.Unsafe (unsafePartial)
import Unsafe.Coerce (unsafeCoerce)

foreign import libxmljs_version :: String
foreign import libxml_version :: String
foreign import libxml_parser_version :: String

foreign import data Document :: Type

foreign import data RawNode :: Type -> Type
foreign import data RawElementNode :: Type
foreign import data RawCommentNode :: Type
foreign import data RawAttributeNode :: Type
foreign import data RawTextNode :: Type
foreign import data RawDTDNode :: Type
foreign import data RawCDataNode :: Type
foreign import data RawPINode :: Type

type Node a = RawNode a
type Element = RawNode RawElementNode
type Comment = RawNode RawCommentNode
type Attribute = RawNode RawAttributeNode
type Text = RawNode RawTextNode
type DTD = RawNode RawDTDNode
type CData = RawNode RawCDataNode
type ProcessingInstruction = RawNode RawPINode

data NodeType = Comment | Element | Text | Attribute | DTD | CData | ProcessingInstruction
derive instance eqNodeType :: Eq NodeType
derive instance genericNodeType :: Generic NodeType _
instance showNodeType :: Show NodeType where
  show = genericShow

foreign import _parseXmlString :: EffectFn1 String Document
foreign import _parseHtmlString :: EffectFn1 String Document
foreign import _parseHtmlFragment :: EffectFn1 String Document

parseXmlString :: String -> Effect (Either Error Document)
parseXmlString string = try $ runEffectFn1 _parseXmlString string

parseHtmlString :: String -> Effect (Either Error Document)
parseHtmlString string = try $ runEffectFn1 _parseHtmlString string

parseHtmlFragment :: String -> Effect (Either Error Document)
parseHtmlFragment string = try $ runEffectFn1 _parseHtmlFragment string

foreign import memoryUsage :: Effect Int
foreign import nodeCount :: Effect Int

foreign import _nodeType :: forall a. Node a -> String
foreign import _nodeDoc :: forall a. Node a -> Effect (Nullable Document)
foreign import _nodeParent :: forall a. Node a -> Effect (Nullable (Node Unit))
foreign import _prevSibling :: forall a. Node a -> Effect (Nullable (Node Unit))
foreign import _nextSibling :: forall a. Node a -> Effect (Nullable (Node Unit))
foreign import nodeLine :: forall a. Node a -> Effect Int
foreign import nodeRemove :: forall a. Node a -> Effect Unit
foreign import nodeClone :: forall a. Node a -> Effect (Node a)
foreign import nodeToString :: forall a. Node a -> Effect String

nextSibling :: forall a. Node a -> Effect (Maybe (Node Unit))
nextSibling n = toMaybe <$> _nextSibling n

prevSibling :: forall a. Node a -> Effect (Maybe (Node Unit))
prevSibling n = toMaybe <$> _prevSibling n

nodeParent :: forall a. Node a -> Effect (Maybe (Node Unit))
nodeParent n = toMaybe <$> _nodeParent n

nodeDoc :: forall a. Node a -> Effect (Maybe Document)
nodeDoc n = toMaybe <$> _nodeDoc n

nodeType :: forall a. Node a -> NodeType
nodeType node = unsafePartial $ case _nodeType node of
  "comment" -> Comment
  "element" -> Element
  "text" -> Text
  "attribute" -> Attribute
  "dtd" -> DTD
  "cdata" -> CData
  "pi" -> ProcessingInstruction

asComment :: forall a. Node a -> Maybe Comment
asComment n
  | nodeType n == Comment = Just (unsafeCoerce n)
  | otherwise = Nothing

asElement :: forall a. Node a -> Maybe Element
asElement n
  | nodeType n == Element = Just (unsafeCoerce n)
  | otherwise = Nothing

asText :: forall a. Node a -> Maybe Text
asText n
  | nodeType n == Text = Just (unsafeCoerce n)
  | otherwise = Nothing

asAttribute :: forall a. Node a -> Maybe Attribute
asAttribute n
  | nodeType n == Attribute = Just (unsafeCoerce n)
  | otherwise = Nothing

asDTD :: forall a. Node a -> Maybe DTD
asDTD n
  | nodeType n == DTD = Just (unsafeCoerce n)
  | otherwise = Nothing

asCData :: forall a. Node a -> Maybe CData
asCData n
  | nodeType n == CData = Just (unsafeCoerce n)
  | otherwise = Nothing

asProcessingInstruction :: forall a. Node a -> Maybe ProcessingInstruction
asProcessingInstruction n
  | nodeType n == ProcessingInstruction = Just (unsafeCoerce n)
  | otherwise = Nothing

type DocEncodingAndVersion = {encoding :: String, version :: String}

defaultDocEncodingAndVersion :: DocEncodingAndVersion
defaultDocEncodingAndVersion = {encoding: "utf8", version: "1.0"}

foreign import _newDoc :: EffectFn2 String String Document
foreign import docChildNodes :: Document -> Effect (Array (Node Unit))
foreign import docEncoding :: Document -> Effect String
foreign import docVersion :: Document -> Effect String
foreign import docSetEncoding :: String -> Document -> Effect Unit
foreign import docToString :: Document -> Effect String
foreign import docValidate :: Document -> Document -> Effect Unit
foreign import docFind :: String -> Document -> Effect (Array Element)
foreign import _docNode :: EffectFn3 String String Document Element
foreign import _docRoot :: EffectFn1 Document (Nullable Element)
foreign import _docSetRoot :: EffectFn2 Element Document Element
foreign import _docGetDtd :: EffectFn1 Document (Nullable DTD)

newDoc :: DocEncodingAndVersion -> Effect Document
newDoc options = runEffectFn2 _newDoc options.version options.encoding

docCreateRoot :: String -> String -> Document -> Effect Element
docCreateRoot name content document = runEffectFn3 _docNode name content document

docGetRoot :: Document -> Effect (Maybe Element)
docGetRoot document = toMaybe <$> runEffectFn1 _docRoot document

docSetRoot :: Element -> Document -> Effect Unit
docSetRoot root document = void $ runEffectFn2 _docSetRoot root document

docGetDtd :: Document -> Effect (Maybe DTD)
docGetDtd document = toMaybe <$> runEffectFn1 _docGetDtd document

foreign import attrName :: Attribute -> String
foreign import attrValue :: Attribute -> Effect String
foreign import attrSetValue :: String -> Attribute -> Effect Unit
foreign import attrRemove :: Attribute -> Effect Unit

foreign import newComment :: String -> Document -> Effect Comment
foreign import commentText :: Comment -> Effect String
foreign import commentSetText :: String -> Comment -> Effect Unit

foreign import elementAddNode :: String -> String -> Element -> Effect Element
foreign import elementName :: Element -> Effect String
foreign import elementSetName :: String -> Element -> Effect Unit
foreign import elementText :: Element -> Effect String
foreign import elementSetText :: String -> Element -> Effect Unit
foreign import _elementAttr :: String -> Element -> Effect (Nullable Attribute)
foreign import elementSetAttr :: String -> String -> Element -> Effect Unit
foreign import elementAttrs :: Element -> Effect (Array Attribute)
foreign import elementChildNodes :: Element -> Effect (Array (RawNode Unit))
foreign import elementAddChild :: Element -> Element -> Effect Unit
foreign import _elementNextElement :: Element -> Effect (Nullable Element)
foreign import _elementPrevElement :: Element -> Effect (Nullable Element)
foreign import elementAddNextSibling :: Element -> Element -> Effect Unit
foreign import elementAddPrevSibling :: Element -> Element -> Effect Unit
foreign import elementFind :: String -> Element -> Effect (Array (RawNode Unit))
foreign import elementReplaceWithElement :: Element -> Element -> Effect Unit
foreign import elementReplaceWithText :: String -> Element -> Effect Unit
foreign import elementPath :: Element -> Effect String

foreign import dtdName :: DTD -> String
foreign import _dtdExternalId :: DTD -> Nullable String
foreign import _dtdSystemId :: DTD -> Nullable String

dtdExternalId :: DTD -> Maybe String
dtdExternalId dtd = toMaybe $ _dtdExternalId dtd

dtdSystemId :: DTD -> Maybe String
dtdSystemId dtd = toMaybe $ _dtdSystemId dtd
