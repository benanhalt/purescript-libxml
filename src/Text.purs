module Libxml.Text
       (newText
       , textGetText
       , textSetText
       , textAddNextSibling
       , textAddPrevSibling
       ) where

import Libxml.Types

import Effect (Effect)
import Prelude (Unit)

foreign import newText :: String -> Document -> Effect Text
foreign import textSetText :: String -> Text -> Effect Unit
foreign import textGetText :: Text -> Effect String
foreign import textAddNextSibling :: forall a. Node a -> Text -> Effect Unit
foreign import textAddPrevSibling :: forall a. Node a -> Text -> Effect Unit

