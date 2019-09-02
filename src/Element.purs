module Libxml.Element
       ( newElement
       , elementAddNode
       , elementAttr
       , elementSetAttr
       , elementAddChild
       , elementChildNodes
       , elementName
       , elementSetName
       , elementText
       , elementSetText
       , elementAddPrevSibling
       , elementAddNextSibling
       , elementReplaceWithElement
       , elementReplaceWithText
       , elementPath
       )
where

import Prelude
import Libxml.Types

import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import Effect.Uncurried (EffectFn3, runEffectFn3)


foreign import _newElement :: EffectFn3 Document String String Element
foreign import elementAddNode :: String -> String -> Element -> Effect Element
foreign import elementName :: Element -> Effect String
foreign import elementSetName :: String -> Element -> Effect Unit
foreign import elementText :: Element -> Effect String
foreign import elementSetText :: String -> Element -> Effect Unit
foreign import _elementAttr :: String -> Element -> Effect (Nullable Attribute)
foreign import elementSetAttr :: String -> String -> Element -> Effect Unit
foreign import elementAttrs :: Element -> Effect (Array Attribute)
foreign import elementChildNodes :: Element -> Effect (Array (RawNode Unit))
foreign import elementAddChild :: forall a. RawNode a -> Element -> Effect Unit
foreign import _elementNextElement :: Element -> Effect (Nullable Element)
foreign import _elementPrevElement :: Element -> Effect (Nullable Element)
foreign import elementAddNextSibling :: Element -> Element -> Effect Unit
foreign import elementAddPrevSibling :: Element -> Element -> Effect Unit
foreign import elementFind :: String -> Element -> Effect (Array (RawNode Unit))
foreign import elementReplaceWithElement :: Element -> Element -> Effect Unit
foreign import elementReplaceWithText :: String -> Element -> Effect Unit
foreign import elementPath :: Element -> Effect String

newElement :: Document -> String -> String -> Effect Element
newElement doc name content = runEffectFn3 _newElement doc name content

elementAttr :: String -> Element -> Effect (Maybe Attribute)
elementAttr name elem = toMaybe <$> _elementAttr name elem
