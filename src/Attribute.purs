module Libxml.Attribute
       ( attrName
       , attrValue
       , attrSetValue
       , attrRemove
       )
where

import Libxml.Types
import Prelude (Unit)
import Effect (Effect)

foreign import attrName :: Attribute -> String
foreign import attrValue :: Attribute -> Effect String
foreign import attrSetValue :: String -> Attribute -> Effect Unit
foreign import attrRemove :: Attribute -> Effect Unit

