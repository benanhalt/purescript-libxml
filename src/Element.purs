module Libxml.Element
       ( newElement
       , elementAddNode
       , elementAttr
       , elementAttrs
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
       , elementFind
       , elementGetElement
       )
where

import Libxml.Types
import Prelude

import Control.Alt ((<|>))
import Control.Monad.Except (runExcept)
import Data.Array (head)
import Data.Either (fromRight)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toMaybe)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Uncurried (EffectFn3, runEffectFn3)
import Foreign (F, Foreign, readArray, readBoolean, readNullOrUndefined, readNumber, readString, unsafeFromForeign)
import Libxml.Node (asElement)
import Partial.Unsafe (unsafePartial)


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
foreign import _elementFind :: String -> Element -> Effect Foreign
foreign import elementReplaceWithElement :: Element -> Element -> Effect Unit
foreign import elementReplaceWithText :: String -> Element -> Effect Unit
foreign import elementPath :: Element -> Effect String

newElement :: Document -> String -> String -> Effect Element
newElement doc name content = runEffectFn3 _newElement doc name content

elementAttr :: String -> Element -> Effect (Maybe Attribute)
elementAttr name elem = toMaybe <$> _elementAttr name elem

elementGetElement :: String -> Element -> Effect (Maybe Element)
elementGetElement xpath elem = do
  maybeResult <- elementFind xpath elem
  pure do
    result <- maybeResult
    node <- case result of
          NodeSet nodes -> head nodes
          otherwise -> Nothing
    asElement node

elementFind :: String -> Element -> Effect (Maybe XPathResult)
elementFind xpath elem = do
  foreignResult <- _elementFind xpath elem
  pure $ unsafePartial fromRight $ runExcept do
    f <- readNullOrUndefined foreignResult
    traverse readXPathResult $ f

readXPathResult :: Foreign -> F XPathResult
readXPathResult f = (NumberResult <$> readNumber f)
                    <|> (StringResult <$> readString f)
                    <|> (BoolResult <$> readBoolean f)
                    <|> (NodeSet <$> readNodeSet f)

readNode :: Foreign -> F (Node Unit)
readNode f = pure $ unsafeFromForeign f

readNodeSet :: Foreign -> F (Array (Node Unit))
readNodeSet f = traverse readNode =<< readArray f
