module Libxml.Element
       ( elementAddNode
       , elementSetAttr
       , elementAddChild
       , elementChildNodes
       , elementName
       , elementText
       )
where

import Prelude (Unit)
import Libxml.Types

import Data.Nullable (Nullable)
import Effect (Effect)

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
