module Libxml.Node
       ( nodeType
       , nodeParent
       , nodeRemove
       , nodeToString
       , asElement
       )
where

import Prelude (Unit, otherwise, ($), (<$>), (==))
import Libxml.Types

import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import Partial.Unsafe (unsafePartial)
import Unsafe.Coerce (unsafeCoerce)


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

