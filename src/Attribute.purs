module Libxml.Attribute
       ( attrName
       , attrValue
       , attrSetValue
       , attrNode
       , attrPrevSibling
       , attrNextSibling
       )
where

import Libxml.Types
import Prelude

import Data.Maybe (Maybe)
import Effect (Effect)
import Libxml.Node (asAttribute, nextSibling, prevSibling)

foreign import attrName :: Attribute -> String
foreign import attrValue :: Attribute -> Effect String
foreign import attrSetValue :: String -> Attribute -> Effect Unit
foreign import attrNode :: Attribute -> Effect Element

attrPrevSibling :: Attribute -> Effect (Maybe Attribute)
attrPrevSibling attr = do
  prev <- prevSibling attr
  pure $ asAttribute =<< prev

attrNextSibling :: Attribute -> Effect (Maybe Attribute)
attrNextSibling attr = do
  next <- nextSibling attr
  pure $ asAttribute =<< next
