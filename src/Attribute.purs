module Libxml.Attribute
       ( attrName
       , attrValue
       , attrSetValue
       , attrNode
       , attrPrevSibling
       , attrNextSibling
       , attrNameSpace
       , attrSetNameSpace
       )
where

import Libxml.Types
import Prelude

import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import Libxml.Node (asAttribute, nextSibling, prevSibling)

foreign import attrName :: Attribute -> String
foreign import attrValue :: Attribute -> Effect String
foreign import attrSetValue :: String -> Attribute -> Effect Unit
foreign import attrNode :: Attribute -> Effect Element
foreign import attrSetNameSpace :: String -> String -> Attribute -> Effect Unit
foreign import _attrNameSpace :: Attribute -> Effect (Nullable NameSpace)

attrPrevSibling :: Attribute -> Effect (Maybe Attribute)
attrPrevSibling attr = do
  prev <- prevSibling attr
  pure $ asAttribute =<< prev

attrNextSibling :: Attribute -> Effect (Maybe Attribute)
attrNextSibling attr = do
  next <- nextSibling attr
  pure $ asAttribute =<< next

attrNameSpace :: Attribute -> Effect (Maybe NameSpace)
attrNameSpace attr = toMaybe <$> _attrNameSpace attr
