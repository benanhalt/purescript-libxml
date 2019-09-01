module Libxml.Comment (newComment, commentText, commentSetText) where

import Prelude (Unit)
import Libxml.Types

import Effect (Effect)

foreign import newComment :: String -> Document -> Effect Comment
foreign import commentText :: Comment -> Effect String
foreign import commentSetText :: String -> Comment -> Effect Unit
