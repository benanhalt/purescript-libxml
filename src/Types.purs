module Libxml.Types
       ( Document
       , Node
       , Element
       , Comment
       , Attribute
       , Text
       , DTD
       , CData
       , ProcessingInstruction
       , RawNode
       , RawElementNode
       , RawCommentNode
       , RawAttributeNode
       , RawTextNode
       , RawDTDNode
       , RawCDataNode
       , RawPINode
       , NodeType(..)
       , XPathResult(..)
       )
where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

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

data XPathResult = NodeSet (Array (Node Unit))
                 | StringResult String
                 | NumberResult Number
                 | BoolResult Boolean
