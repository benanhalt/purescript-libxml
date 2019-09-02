module Libxml.Text (newText, textGetText) where

import Libxml.Types

import Effect (Effect)
import Prelude (Unit)

foreign import newText :: String -> Document -> Effect Text
foreign import textSetText :: String -> Text -> Effect Unit
foreign import textGetText :: Text -> Effect String

